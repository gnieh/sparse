package sparse
package driver

import scala.reflect.macros.whitebox.Context

import scala.language.experimental.macros
import scala.language.reflectiveCalls

import scala.annotation.tailrec

import scala.collection.immutable.Queue

class ParserImpl(val c: Context) {

  import c.universe._

  // EBNF-style Ast with high-level rule operators
  sealed trait Ebnf
  case class SeqEbnf(seq: List[Ebnf]) extends Ebnf
  case class AltEbnf(alternatives: List[Ebnf]) extends Ebnf
  case class SemEbnf(left: Ebnf, transform: Function) extends Ebnf
  case class OptEbnf(left: Ebnf) extends Ebnf
  case class StarEbnf(left: Ebnf) extends Ebnf
  case class PlusEbnf(left: Ebnf) extends Ebnf
  case object EmptyEbnf extends Ebnf
  case class TerminalEbnf(tpe: Symbol) extends Ebnf
  case class NonTerminalEbnf(path: Symbol) extends Ebnf

  // BNF-style Ast which consists only of sequences of terminals and non-terminals
  sealed trait Bnf {
    def +(that: Bnf): Bnf
  }
  case class SeqBnf(sequence: List[NameBnf]) extends Bnf {
    def +(that: Bnf): Bnf = that match {
      case SeqBnf(seq) => SeqBnf(sequence ++ seq)
      case EmptyBnf => this
    }
  }
  case object EmptyBnf extends Bnf {
    def +(that: Bnf): Bnf = that
  }
  sealed trait NameBnf
  case class TerminalBnf(tpe: Symbol) extends NameBnf
  case class NonTerminalBnf(tpe: Symbol) extends NameBnf


  // ========== Ebnf construction ==========

  def rhsEbnf(t: Tree): Ebnf = t match {
    case q"$left.~[$t]($right)" => seq(rhsEbnf(left), rhsEbnf(right))
    case q"$left.|[$t]($right)" => alt(rhsEbnf(left), rhsEbnf(right))
    case q"$left.~>[$t](($in) => $out)"  => SemEbnf(rhsEbnf(left), q"($in) => $out")
    case q"$left.?" => opt(rhsEbnf(left))
    case q"$left.*" => star(rhsEbnf(left))
    case q"$left.+" => plus(rhsEbnf(left))
    case q"$prefix.term[$t]" => TerminalEbnf(t.symbol)
    case q"$prefix.empty" => EmptyEbnf
    case q"$path.$id" if t.tpe <:< c.typeOf[sparse.Symbol[Any]] => NonTerminalEbnf(t.symbol)
    case q"$id" if t.tpe <:< c.typeOf[sparse.Symbol[Any]] => NonTerminalEbnf(t.symbol)
    case _ =>
      c.error(t.pos, "Only rule definitions may appear in the `Grammar` section.")
      // return dummy empty bnf to continue transforming and potentially finding other errors
      EmptyEbnf
  }

  def seq(left: Ebnf, right: Ebnf): Ebnf = (left, right) match {
    case (EmptyEbnf, _)                 => right
    case (_, EmptyEbnf)                 =>  left
    case (SeqEbnf(seq1), SeqEbnf(seq2)) => SeqEbnf(seq1 ++ seq2)
    case (SeqEbnf(seq), _)              => SeqEbnf(seq :+ right)
    case (_, SeqEbnf(seq))              => SeqEbnf(left +: seq)
    case (_, _)                         => SeqEbnf(List(left, right))
  }

  def alt(left: Ebnf, right: Ebnf): AltEbnf = (left, right) match {
    case (AltEbnf(alts1), AltEbnf(alts2)) => AltEbnf(alts1 ++ alts2)
    case (AltEbnf(alts), t)               => AltEbnf(alts :+ t)
    case (t, AltEbnf(alts))               => AltEbnf(t +: alts)
    case (t1, t2)                         => AltEbnf(List(t1, t2))
  }

  def opt(left: Ebnf) = left match {
    case StarEbnf(s) => StarEbnf(s)
    case PlusEbnf(p) => StarEbnf(p)
    case OptEbnf(o)  => OptEbnf(o)
    case t           => OptEbnf(t)
  }

  def star(left: Ebnf) = left match {
    case StarEbnf(s) => StarEbnf(s)
    case PlusEbnf(p) => StarEbnf(p)
    case OptEbnf(o)  => StarEbnf(o)
    case t           => StarEbnf(t)
  }

  def plus(left: Ebnf) = left match {
    case StarEbnf(s) => StarEbnf(s)
    case PlusEbnf(p) => PlusEbnf(p)
    case OptEbnf(o)  => StarEbnf(o)
    case t           => PlusEbnf(t)
  }

  // ========== Ebnf normlization ==========

  /** Extracts high-level constructs, i.e.:
   *   - PlusEbnf
   *   - StarEbnf
   *   - OptEbnf
   */
  @tailrec
  final def extractHighLevel(queue: Queue[Symbol], rules: Map[Symbol,Ebnf]): Map[Symbol,Ebnf] = queue.dequeueOption match {
    case Some((name, rest)) =>
      def doOne(topLevel: Boolean, ebnf: Ebnf, queue: Queue[Symbol], rules:Map[Symbol,Ebnf]): (Ebnf, Queue[Symbol], Map[Symbol,Ebnf]) = ebnf match {
        case PlusEbnf(inner) if topLevel =>
          val (inner1, queue1, rules1) = doOne(false, inner, queue, rules)
          (AltEbnf(List(inner1, SeqEbnf(List(inner1, NonTerminalEbnf(name))))), queue1, rules1)
        case StarEbnf(inner) if topLevel =>
          val (inner1, queue1, rules1) = doOne(false, inner, queue, rules)
          (AltEbnf(List(EmptyEbnf, SeqEbnf(List(inner1, NonTerminalEbnf(name))))), queue1, rules1)
        case OptEbnf(inner) =>
          val (inner1, queue1, rules1) = doOne(false, inner, queue, rules)
          (AltEbnf(List(EmptyEbnf, inner1)), queue1, rules1)
        case PlusEbnf(_) | StarEbnf(_) =>
          // not top-level, create a new rule and add it to the queue
          val newName = newSymbol(name)
          (NonTerminalEbnf(newName), queue.enqueue(newName), rules.updated(newName, ebnf))
        case SeqEbnf(seq) =>
          val (seq1, queue1, rules1) = seq.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (SeqEbnf(seq1), queue1, rules1)
        case AltEbnf(alts) =>
          val (alts1, queue1, rules1) = alts.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (AltEbnf(alts1), queue1, rules1)
        case SemEbnf(inner, f) =>
          val (inner1, queue1, rules1) = doOne(false, inner, queue, rules)
          (SemEbnf(inner1, f), queue1, rules1)
        case EmptyEbnf | TerminalEbnf(_) | NonTerminalEbnf(_) =>
          (ebnf, queue, rules)
      }
      val (ebnf1, queue1, rules1) = doOne(true, rules(name), rest, rules)
      val rules2 = rules1.updated(name, ebnf1)
      extractHighLevel(queue1, rules2)
    case None =>
      rules
  }

  /** Extracts the semantic actions to make them top-level only. */
  @tailrec
  final def extractActions(queue: Queue[Symbol], rules: Map[Symbol,Ebnf]): Map[Symbol,Ebnf] = queue.dequeueOption match {
    case Some((name, rest)) =>
      def doOne(topLevel: Boolean, ebnf: Ebnf, queue: Queue[Symbol], rules: Map[Symbol,Ebnf]): (Ebnf, Queue[Symbol], Map[Symbol,Ebnf]) = ebnf match {
        case SemEbnf(inner, f) if topLevel =>
          val (inner1, queue1, rules1) = doOne(false, inner, queue, rules)
          (SemEbnf(inner1, f), queue1, rules1)
        case SemEbnf(inner, f) =>
          // not top-level, create a new rule and add it to the queue
          val newName = newSymbol(name)
          (NonTerminalEbnf(newName), queue.enqueue(newName), rules.updated(newName, ebnf))
        case SeqEbnf(seq) =>
          val (seq1, queue1, rules1) = seq.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (SeqEbnf(seq1), queue1, rules1)
        case AltEbnf(alts) =>
          val (alts1, queue1, rules1) = alts.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (AltEbnf(alts1), queue1, rules1)
        case EmptyEbnf | TerminalEbnf(_) | NonTerminalEbnf(_) =>
          (ebnf, queue, rules)
        case _ =>
          c.abort(c.enclosingPosition, "Bad Ebnf shape (this is a bug)")
      }
      val (ebnf1, queue1, rules1) = doOne(true, rules(name), rest, rules)
      val rules2 = rules1.updated(name, ebnf1)
      extractActions(queue1, rules2)
    case None =>
      rules
  }

  /** Extracts the alternatives to make them top-level only. */
  @tailrec
  final def extractAlternatives(queue: Queue[Symbol], rules: Map[Symbol,Ebnf]): Map[Symbol,Ebnf] = queue.dequeueOption match {
    case Some((name, rest)) =>
      def doOne(topLevel: Boolean, ebnf: Ebnf, queue: Queue[Symbol], rules:Map[Symbol,Ebnf]): (Ebnf, Queue[Symbol], Map[Symbol,Ebnf]) = ebnf match {
        case AltEbnf(alts) if topLevel =>
          val (alts1, queue1, rules1) = alts.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (AltEbnf(alts1), queue1, rules1)
        case AltEbnf(_) =>
          // not top-level, create a new rule and add it to the queue
          val newName = newSymbol(name)
          (NonTerminalEbnf(newName), queue.enqueue(newName), rules.updated(newName, ebnf))
        case SemEbnf(inner, f) =>
          val (inner1, queue1, rules1) = doOne(topLevel, inner, queue, rules)
          (SemEbnf(inner1, f), queue1, rules1)
        case SeqEbnf(seq) =>
          val (seq1, queue1, rules1) = seq.foldRight((List[Ebnf](), queue, rules)) {
            case (ebnf, (acc, queue, rules)) =>
              val (ebnf1, queue1, rules1) = doOne(false, ebnf, queue, rules)
              (ebnf1 :: acc, queue1, rules1)
          }
          (SeqEbnf(seq1), queue1, rules1)
        case EmptyEbnf | TerminalEbnf(_) | NonTerminalEbnf(_) =>
          (ebnf, queue, rules)
        case _ =>
          c.abort(c.enclosingPosition, "Bad Ebnf shape (this is a bug)")
      }
      val (ebnf1, queue1, rules1) = doOne(true, rules(name), rest, rules)
      val rules2 = rules1.updated(name, ebnf1)
      extractAlternatives(queue1, rules2)
    case None =>
      rules
  }

  def ebnf2bnf(rules: Map[Symbol,Ebnf]): Map[Symbol,(Set[Bnf],Option[Function])] = {
    val rules1 = extractHighLevel(iterable2queue(rules.keys), rules)
    val rules2 = extractActions(iterable2queue(rules1.keys), rules1)
    val rules3 = extractAlternatives(iterable2queue(rules2.keys), rules2)

    def doName(ebnf:Ebnf): NameBnf = ebnf match {
      case TerminalEbnf(name)    => TerminalBnf(name)
      case NonTerminalEbnf(name) => NonTerminalBnf(name)
      case _                     => c.abort(c.enclosingPosition, "Bad Ebnf shape (this is a bug)")
    }

    def doOne(ebnf: Ebnf): Set[Bnf] = ebnf match {
      case SeqEbnf(seq) =>
        Set(SeqBnf(seq.map(doName)))
      case AltEbnf(alts) =>
        alts.foldLeft(Set.empty[Bnf]) { (acc, alt) => acc ++ doOne(alt) }
      case EmptyEbnf =>
        Set(EmptyBnf)
      case TerminalEbnf(name) =>
        Set(SeqBnf(List(TerminalBnf(name))))
      case NonTerminalEbnf(name) =>
        Set(SeqBnf(List(NonTerminalBnf(name))))
      case _ =>
        c.abort(c.enclosingPosition, "Bad Ebnf shape (this is a bug)")
    }

    for((name, ebnf) <- rules3)
      yield ebnf match {
        case SemEbnf(inner, f) => (name, (doOne(inner), Some(f)))
        case _                 => (name, (doOne(ebnf), None))
      }
  }

  // ========== Utilities ==========

  private def iterable2queue[T](it: Iterable[T]): Queue[T] =
    it.foldLeft(Queue.empty[T])(_.enqueue(_))

  private def newSymbol(base: Symbol): Symbol = {
    val tpe = tq"${base.typeSignature}"
    c.typecheck(q"val ${TermName(c.freshName(base.name.toString))}: $tpe = ???").symbol
  }

  // ========== Public API ==========

  def Grammar(rules: Tree): Tree = {

    val ebnfizer = new Traverser {

      var rules = Map.empty[Symbol,Ebnf]

      override def traverse(tree: c.Tree): Unit = tree match {
        case q"$_ val $ruleName:$t = $v" if t.tpe <:< c.typeOf[sparse.Symbol[Any]] =>
          val ebnf = rhsEbnf(v)
          rules += (tree.symbol -> ebnf)
        case q"$_ def $ruleName:$t = $v" if t.tpe <:< c.typeOf[sparse.Symbol[Any]] =>
          val ebnf = rhsEbnf(v)
          rules += (tree.symbol -> ebnf)
        case ClassDef(_, _, _, _) | ModuleDef(_, _, _) | DefDef(_, _, _, _, _, _) | ValDef(_, _, _, _) =>
          c.error(tree.pos, "Only rule definitions may appear in the `Grammar` section.")
        case _ =>
          super.traverse(tree)
      }
    }

    // transform AST to EBNF rules
    ebnfizer.traverse(rules)

    // extract canonical BNF rules
    val bnf = ebnf2bnf(ebnfizer.rules)

    println(bnf)

    // build the LALR(1) automaton for the rules

    q"???"
  }

}
