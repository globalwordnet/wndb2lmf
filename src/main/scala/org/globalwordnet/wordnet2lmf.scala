package org.globalword.wndb2lmf

import scala.io.{Source}
import scala.xml.{NodeSeq,Elem,Null,TopScope,PrefixedAttribute}
import java.net.URLEncoder
import java.io.{PrintWriter,File}
import org.apache.commons.lang3.StringEscapeUtils.{escapeXml10 => escapeXml}

object wordnet2lemon {

  def main(args : Array[String]) {
    val wnFolder = if(args.length == 0) {
      "/home/jmccrae/Downloads/dict/"
//      "/home/jmccrae/Downloads/WordNet-3.0/dict/"
    } else {
      args(0)
    }

    val (wnid, title, language, iliRef, output) = if(args.length == 5) {
      (args(0), args(1), args(2), args(3), args(4))
    } else {
      ("wn31", "WordNet", "en", "../ili/ili-wn31.ttl", "wn31.xml")
    }

    
    def words(pos : String) : Iterator[WordNetDataItem] = if(new File(wnFolder+"data."+pos).exists) {
      for(line <- Source.fromFile(wnFolder+"data."+pos).getLines() if !(line matches "\\s+.*")) yield {
        WordNetDataItem.fromString(line)
      }
    } else {
      Nil.iterator
    }
    
    val excVerbs = excForms(wnFolder + "verb.exc")
    val excNouns = excForms(wnFolder + "noun.exc")
    val excAdjs = excForms(wnFolder + "adj.exc")
    val excAdvs = excForms(wnFolder + "adv.exc")
    val sentences = loadSentences(wnFolder + "sents.vrb")
    
    val iliMap = loadILIMap(iliRef)
    
    val items = words("verb").toSeq ++ words("noun").toSeq ++ words("adj").toSeq ++ words("adv").toSeq
    
    //println("Building satellites")
    //val satellites = buildSatellites(items)
    //println("Done with satellites")

    //val senseIdx = buildIndex(items, satellites)
    val out = new PrintWriter(output)
    buildLMF(out, items, wnid, title, language, sentences, iliMap)
    out.flush
    out.close

//    dumpDefinitions(items)
  }
  
  def excForms(file : String) : Map[String,Seq[(String,String)]] = {
    (for(line <- Source.fromFile(file).getLines()) yield {
      line.split(" ") match {
        case Array(variant,lemma) => Some(lemma -> variant)
        case _ => None
      }
    }).flatten.toSeq.groupBy(_._1)
  }

  def buildSatellites(items : Seq[WordNetDataItem]) = {
    val it2 = items.filter(_.pos == Adjective)
    (for(item <- it2) yield {
      item.offset -> (item.lemmas(0).lemma, item.lexNo)
    }).toMap
  }


  def buildIndex(items : Seq[WordNetDataItem], satellites : Map[Int,(String,Int)]) = (
    for(item <- items ; word <- item.lemmas) yield {
      Seq((item.offset + ":" + item.pos.shortForm + ":" + word.synNo) -> word.senseIdxEscaped(satellites),
         (item.offset + "/" + item.pos.shortForm + "/" + word.synNo) -> (urlencode(word.lemma) + ":" + item.pos.shortForm))
    }
  ).flatten.toMap

  def buildLMF(out : PrintWriter, items : Seq[WordNetDataItem], id : String, 
    label : String, language : String, sentences : Map[Int, String],
    ili : Map[(Int, String), String]) {
    out.print(s"""<LexicalResource>
  <Lexicon id="$id" label="$label" language="$language">""")

    buildEntries(out, items, id, sentences)
    buildSynsets(out, items, id, language, ili)
out.println("""  </Lexicon>
</LexicalResource>""")
  }

  def buildEntries(out : PrintWriter, items : Seq[WordNetDataItem], id : String,
    sentences : Map[Int, String])  {
    val map = collection.mutable.HashMap[String, Seq[WordNetDataItem]]()
    for(item <- items) {
      for(lemma <- item.lemmas) {
        val lemma_key = lemma.lemma + "-" + item.pos.shortForm
        if(!(map contains lemma_key)) {
          map.put(lemma_key, Seq(item))
        } else{
          map.put(lemma_key, map(lemma_key) :+ item)
        }
      }
    }
    for((lemma_key, items) <- map) {
      val lemma = lemma_key.dropRight(2)
      val pos = items(0).pos 
      out.print(s"""
    <LexicalEntry id="${urlencode(id + "-" + lemma_key)}">
      <Lemma writtenForm="${escapeXml(lemma)}" pos="${pos.shortForm}"/>""")
      for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items) {
        val word = lemmas.find(_.lemma == lemma).get
        val srcIdx = word.synNo
        out.print(s"""
      <Sense id="${"%s-%08d-%s-%d" format (id, offset, pos.shortForm, srcIdx)}"
             synset="${"%s-$08d-%s" format (id, offset, pos.shortForm)}">""")
        for(Pointer(typ, targetOffset, pos, src, trg) <- pointers if srcIdx == src) {
          out.print(s"""
        <SenseRelation target="${"%s-%08d-%s-%d" format (id, targetOffset, pos.shortForm, trg)}" relType="${typ.name}"/>""")
        }
        out.print("""
      </Sense>""")
      }
      val frames = items.flatMap(_.frames).toSet
      for(frame <- frames) {
        out.print(s"""
      <SyntacticBehaviour subcategorizationFrame="${sentences.getOrElse(frame.frameId, "ERROR")}"/>""") 
      }
      out.print(s"""
    </LexicalEntry>""")
    }
  }

  def buildSynsets(out : PrintWriter, items : Seq[WordNetDataItem], id : String, language : String,
      ili : Map[(Int, String), String])  {
    for(WordNetDataItem(offset, lexNo, pos, lemmas, pointers, frames, gloss) <- items) {
      val iliId = ili.get((offset, pos.shortForm)) match {
        case Some(id) =>
          id
        case None =>
          System.err.println("new,,,%08d-%s,%s" format (offset, pos.shortForm, lemmas.map(_.lemma).mkString("/")))
          "in"
      }
      out.print(s"""
    <Synset id="${"%s-%08d-%s" format (id, offset, pos.shortForm)}"
            ili="${iliId}">
      <Definition gloss="${escapeXml(gloss)}"/>""")
         
        for(Pointer(typ, targetOffset, pos, src, trg) <- pointers if typ.semantic && src == 0 && trg == 0) yield {
          out.print(s"""
      <SynsetRelation target="${"%s-%08d-%s" format (id, targetOffset, pos.shortForm)}"
                      relType="${typ.name}"/>""")
        }
    out.print("""
    </Synset>""")

    }

  }

  def urlencode(s : String) = URLEncoder.encode(s,"UTF-8")

  def loadSentences(fileName : String) : Map[Int, String] = {
    (io.Source.fromFile(fileName).getLines.map { line =>
      val (id, sentence) = line.splitAt(line.indexOf(" "))
      id.toInt -> sentence.drop(1)
    }).toMap
  }

  def loadILIMap(fileName : String) : Map[(Int, String), String] = {
    (io.Source.fromFile(fileName).getLines.drop(5).flatMap { line =>
      val elems = line.split("\\s+")
      val ili = elems(0).drop(4)
      val offset = elems(2).drop(7).dropRight(2).toInt
      val pos = elems(2).takeRight(1)
      if(elems(1) == "owl:sameAs") {
        if(pos == "s") {
          Some((offset, "a") -> ili)
        } else {
          Some((offset, pos) -> ili)
        }
      } else {
        None
      }
    }).toMap
  }

  def dumpDefinitions(items : Seq[WordNetDataItem]) {
    val out = new java.io.PrintWriter("defs-wn30.csv")
    for(WordNetDataItem(offset, _, pos, _, _, _, gloss) <- items) {
      out.println("%s,%08d,%s" format (gloss.replaceAll(",",""), offset, pos.shortForm))
    }
    out.flush
    out.close
  }
}

sealed trait POS {
  def shortForm : String
  def code : String
}

object Noun extends POS {
  val shortForm = "n"
  val code = "1"
}

object Verb extends POS {
  val shortForm = "v"
  val code = "2"
}

object Adjective extends POS {
  val shortForm = "a"
  val code = "3"
}

object AdjectiveSatellite extends POS {
  val shortForm = "a"
  val code ="5"
}

object Adverb extends POS {
  val shortForm = "r"
  val code = "4"
}

object POS {
  def mk(s : String) = s match {
    case "n" => Noun
    case "v" => Verb
    case "a" => Adjective
    case "s" => AdjectiveSatellite
    case "r" => Adverb
  }
}
  
case class WordNetDataItem(val offset : Int, val lexNo : Int,
     val pos : POS, val lemmas : Seq[Word],
     val pointers : Seq[Pointer], val frames : Seq[Frame],
     val gloss : String) {
}
     
object WordNetDataItem {
  def fromString(s : String) = s.split("\\| ") match {
    case Array(data,gloss) => {
      readData(data.split(" ")) match {
        case (o,l,p,ls,ps,fs) => WordNetDataItem(o,l,p,ls,ps,fs,gloss.trim())
      }
    }
    case Array(data) => {
      readData(data.split(" ")) match {
        case (o,l,p,ls,ps,fs) => WordNetDataItem(o,l,p,ls,ps,fs,"")
      }
    }
    case _ => { throw new RuntimeException("Did not understand: " + s) }
  }
  
  private def readData(d : Seq[String]) = {
     val w_cnt = Integer.parseInt(d(3),16)
     val w_end = 4 + w_cnt * 2
     val p_cnt = d(w_end).toInt
     val p_end = w_end + 1 + 4 * p_cnt
     val f_cnt = if(p_end < d.size) {
       d(p_end).toInt
     } else {
       0
     }
     val f_end = p_end + 1 + 3 * f_cnt
     val pos = POS.mk(d(2))
     val lexNo = d(1).toInt
     val ptrs = readPointers(d.slice(w_end + 1, p_end),d(2)).toSeq 
     val head = if(pos == AdjectiveSatellite) {
       getHead(ptrs)
     } else {
       None
     }
     val rval = (d(0).toInt, lexNo,pos,
        readWords(d.slice(4,w_end),lexNo,pos,head).toSeq,
        ptrs, 
        if(p_end < d.size) { readFrames(d.slice(p_end + 1, f_end)).toSeq } else { Nil })
     rval
  }
  
  private def getHead(ptrs : Seq[Pointer]) : Option[Int] = {
    ptrs.find(_.typ == SimilarTo) match {
      case Some(Pointer(_,offset,_,_,_)) => Some(offset)
      case _ => None
    }
  }

  private def readWords(d : Seq[String], lexNo : Int, pos : POS, head : Option[Int]) = {
      for((word,idx) <- d.grouped(2).zipWithIndex) yield {
        Word(word(0).replaceAll("_"," "),Integer.parseInt(word(1),16),idx+1,lexNo,pos,head) 
      }
  }
  
  private def readPointers(d : Seq[String], pos : String) = {
    for(ptr <- d.grouped(4)) yield {
      Pointer(PointerType.mk(ptr(0),pos), ptr(1).toInt,
        POS.mk(ptr(2)), (Integer.parseInt(ptr(3),16) & 0xff00) >> 8, Integer.parseInt(ptr(3),16) & 0x00ff)
    }
  }
  
  private def readFrames(d : Seq[String]) = {
    for(f <- d.grouped(3)) yield {
      Frame(f(1).toInt,Integer.parseInt(f(2),16))
    }
  }
}

case class Word(val lemma : String, val lexId : Int, val synNo : Int, lexNo : Int, pos : POS, head : Option[Int]) {
  def urlencode(s : String) = URLEncoder.encode(s,"UTF-8")
  
  //def senseIdx = lemma.replaceAll(" ","_") + "-" + pos.shortForm + "#" + pos.code + ":" + lexNo + ":" + lexId + "::"
  def senseIdx(sattelites : Map[Int,(String,Int)]) = {
    if(head != None) {
      "%s-%s#%s:%02d:%02d:%s:%02d" format (lemma.replaceAll(" ","_"), pos.shortForm, pos.code, lexNo, lexId, sattelites(head.get)._1,
      sattelites(head.get)._2)
    } else {
      "%s-%s#%s:%02d:%02d::" format (lemma.replaceAll(" ","_"), pos.shortForm, pos.code, lexNo, lexId)
    }
  }
  //def senseIdxEscaped = urlencode(lemma.replaceAll(" ","_")) + "-" + pos.shortForm + "#" + pos.code + ":" + lexNo + ":" + lexId + "::"
  def senseIdxEscaped (sattelites : Map[Int,(String,Int)]) = {
    if(head != None) {
      "%s-%s#%s:%02d:%02d:%s:%02d" format (urlencode(lemma.replaceAll(" ","_")), pos.shortForm, pos.code, lexNo, lexId, sattelites(head.get)._1,
      sattelites(head.get)._2)
    } else {
      "%s-%s#%s:%02d:%02d::" format (urlencode(lemma.replaceAll(" ","_")), pos.shortForm, pos.code, lexNo, lexId)
    }
  }
}

case class Pointer(val typ : PointerType, val targetOffset : Int,
     val pos : POS, val src : Int, val trg : Int)
     
case class Frame(val frameId : Int, val wordId : Int)

sealed trait PointerType {
  def name : String 
  def semantic : Boolean = true
}


object Antonym extends PointerType {
  val name = "antonym"
  override val semantic = false
}
object Hypernym extends PointerType {
  val name = "hypernym"
}
object InstanceHypernym extends PointerType {
  val name = "instance_hypernym"
}
object Hyponym extends PointerType {
  val name = "hyponym"
}
object InstanceHyponym extends PointerType {
  val name = "instance_hyponym"
}
object MemberHolonym extends PointerType {
  val name = "member_holonym"
}
object SubstanceHolonym extends PointerType {
  val name = "substance_holonym"
}
object PartHolonym extends PointerType {
  val name = "part_holonym"
}
object MemberMeronym extends PointerType {
  val name = "member_meronym"
}
object SubstanceMeronym extends PointerType {
  val name = "substance_meronym"
}
object PartMeronym extends PointerType {
  val name = "part_meronym"
}
object Attribute extends PointerType {
  val name = "attribute"
}
object DerivationallyRelatedForm extends PointerType {
  val name = "derivation"
  override def semantic = false
}
object DomainOfSynsetTopic extends PointerType {
  val name = "domain_category"
}
object MemberOfThisDomainTopic  extends PointerType {
  val name = "domain_member_category"
}
object DomainOfSynsetRegion extends PointerType {
  val name = "domain_region"
}
object MemberOfThisDomainRegion  extends PointerType {
  val name = "domain_member_region"
}
object DomainOfSynsetUsage extends PointerType  {
  val name = "domain_usage"
}
object MemberOfThisDomainUsage extends PointerType {
  val name = "domain_member_usage"
}
object Entailment extends PointerType {
  val name = "entail"
}
object Cause extends PointerType {
  val name = "cause"
}
object AlsoSee extends PointerType {
  val name = "also"
  override val semantic = false
}
object VerbGroup extends PointerType {
  val name = "verb_group"
  override val semantic = false
}
object SimilarTo extends PointerType {
  val name = "similar"
}
object ParticipleOfVerb extends PointerType {
  val name = "participle"
  override def semantic = false
}
object Pertainym extends PointerType {
  val name = "pertainym"
}
object DerivedFrom extends PointerType {
  val name = "derivation"
}

object PointerType {
  var nonStandardRels = Set[String]()
  def mk(s : String, pos : String) = s match {
    case "!" => Antonym 
    case "@" => Hypernym 
    case "@i" => InstanceHypernym 
    case "~" => Hyponym 
    case "~i" => InstanceHyponym 
    case "#m" => MemberHolonym 
    case "#s" => SubstanceHolonym 
    case "#p" => PartHolonym 
    case "%m" => MemberMeronym 
    case "%s" => SubstanceMeronym 
    case "%p" => PartMeronym 
    case "=" => Attribute 
    case "+" => DerivationallyRelatedForm         
    case ";c" => DomainOfSynsetTopic
    case "-c" => MemberOfThisDomainTopic
    case ";r" => DomainOfSynsetRegion
    case "-r" => MemberOfThisDomainRegion
    case ";u" => DomainOfSynsetUsage
    case "-u" => DomainOfSynsetUsage
    case "*" =>  Entailment 
    case ">" => Cause
    case "^" => AlsoSee 
    case "$" => VerbGroup 
    case "&" => SimilarTo 
    case "<" => ParticipleOfVerb 
    case "\\" => if(pos == "a" || pos == "s" || pos == "n") {
        Pertainym
    } else if(pos == "r") {
        DerivedFrom
    } else throw new IllegalArgumentException("pos="+pos)
    // I believe these are from plWordNet
    //case "prvdn" => new PointerType { val uri = ("lwn","prvdn") }
    //case "ank" => new PointerType { val uri = ("lwn","ank") }
    //case "caprdn" => new PointerType { val uri = ("lwn","caprdn") }
    //case "zwr" => new PointerType { val uri = ("lwn","zwr") }
    //case "fuzs" => new PointerType { val uri = ("lwn","fuzs") }
    //case "stvn" => new PointerType { val uri = ("lwn","stvn") }
    //case "r?" => new PointerType { val uri = ("lwn","rQ") }
    //case "comp" => new PointerType { val uri = ("lwn","comp") }
    //case "zm" => new PointerType { val uri = ("lwn","zm") }
    //case "rn" => new PointerType { val uri = ("lwn","rn") }
    //case "aan" => new PointerType { val uri = ("lwn","aan") }
    //case "rw" => new PointerType { val uri = ("lwn","rw") }
    //case "wzaj" => new PointerType { val uri = ("lwn","wzaj") }
    //case "svpa" => new PointerType { val uri = ("lwn","svpa") }
    //case "inchn" => new PointerType { val uri = ("lwn","inchn") }
    //case "smprel" => new PointerType { val uri = ("lwn","smprel") }
    //case "sim" => new PointerType { val uri = ("lwn","sim") }
    //case "uprz" => new PointerType { val uri = ("lwn","uprz") }
    //case "vna" => new PointerType { val uri = ("lwn","vna") }
    //case "pvnn" => new PointerType { val uri = ("lwn","pvnn") }
    //case "war" => new PointerType { val uri = ("lwn","war") }
    //case "presup" => new PointerType { val uri = ("lwn","presup") }
    //case "#t" => new PointerType { val uri = ("lwn","Ht") }
    //case "nd" => new PointerType { val uri = ("lwn","nd") }
    //case "ran" => new PointerType { val uri = ("lwn","ran") }
    //case "ascn" => new PointerType { val uri = ("lwn","ascn") }
    //case "f" => new PointerType { val uri = ("lwn","f") }
    //case "egz" => new PointerType { val uri = ("lwn","egz") }
    //case "itern" => new PointerType { val uri = ("lwn","itern") }
    //case "inchd" => new PointerType { val uri = ("lwn","inchd") }
    //case "predoce" => new PointerType { val uri = ("lwn","predoce") }
    //case "a" => new PointerType { val uri = ("lwn","a") }
    //case "zp" => new PointerType { val uri = ("lwn","zp") }
    //case "ravm" => new PointerType { val uri = ("lwn","ravm") }
    //case "hst" => new PointerType { val uri = ("lwn","hst") }
    //case "mpod" => new PointerType { val uri = ("lwn","mpod") }
    //case "m" => new PointerType { val uri = ("lwn","m") }
    //case "castdn" => new PointerType { val uri = ("lwn","castdn") }
    //case "rm" => new PointerType { val uri = ("lwn","rm") }
    //case "%t" => new PointerType { val uri = ("lwn","Pt") }
    //case "char" => new PointerType { val uri = ("lwn","char") }
    //case "%w" => new PointerType { val uri = ("lwn","Pw") }
    //case "%c" => new PointerType { val uri = ("lwn","Pc") }
    //case "apav" => new PointerType { val uri = ("lwn","apav") }
    //case "prvda" => new PointerType { val uri = ("lwn","prvda") }
    //case "sn" => new PointerType { val uri = ("lwn","sn") }
    //case "-" => new PointerType { val uri = ("lwn","D") }
    //case "caprna" => new PointerType { val uri = ("lwn","caprna") }
    //case "zpn" => new PointerType { val uri = ("lwn","zpn") }
    //case "diminaa" => new PointerType { val uri = ("lwn","diminaa") }
    //case "rp" => new PointerType { val uri = ("lwn","rp") }
    //case "aswd" => new PointerType { val uri = ("lwn","aswd") }
    //case "ra" => new PointerType { val uri = ("lwn","ra") }
    //case "ni" => new PointerType { val uri = ("lwn","ni") }
    //case "afk" => new PointerType { val uri = ("lwn","afk") }
    //case "b" => new PointerType { val uri = ("lwn","b") }
    //case "ravcau" => new PointerType { val uri = ("lwn","ravcau") }
    //case "stva" => new PointerType { val uri = ("lwn","stva") }
    //case "ravo" => new PointerType { val uri = ("lwn","ravo") }
    //case "rava" => new PointerType { val uri = ("lwn","rava") }
    //case "caprnn" => new PointerType { val uri = ("lwn","caprnn") }
    //case "aswn" => new PointerType { val uri = ("lwn","aswn") }
    //case "hpo" => new PointerType { val uri = ("lwn","hop") }
    //case "anw" => new PointerType { val uri = ("lwn","anw") }
    //case "dystr" => new PointerType { val uri = ("lwn","dystr") }
    //case "mst" => new PointerType { val uri = ("lwn","mst") }
    //case "zc" => new PointerType { val uri = ("lwn","zc") }
    //case "caprda" => new PointerType { val uri = ("lwn","caprda") }
    //case "typ" => new PointerType { val uri = ("lwn","typ") }
    //case "predhab" => new PointerType { val uri = ("lwn","predhab") }
    //case "?" => new PointerType { val uri = ("lwn","Q") }
    //case "rmn" => new PointerType { val uri = ("lwn","rmn") }
    //case "causdd" => new PointerType { val uri = ("lwn","causdd") }
    //case "causnn" => new PointerType { val uri = ("lwn","causnn") }
    //case "svn" => new PointerType { val uri = ("lwn","svn") }
    //case "c" => new PointerType { val uri = ("lwn","c") }
    //case "rwn" => new PointerType { val uri = ("lwn","rwn") }
    //case "savi" => new PointerType { val uri = ("lwn","savi") }
    //case "sa" => new PointerType { val uri = ("lwn","sa") }
    //case "za" => new PointerType { val uri = ("lwn","za") }
    //case "ascd" => new PointerType { val uri = ("lwn","ascd") }
    //case "sup" => new PointerType { val uri = ("lwn","sup") }
    //case "ne" => new PointerType { val uri = ("lwn","ne") }
    //case "iterd" => new PointerType { val uri = ("lwn","iterd") }
    //case "zn" => new PointerType { val uri = ("lwn","zn") }
    //case "ravczas" => new PointerType { val uri = ("lwn","ravczas") }
    //case "ravres" => new PointerType { val uri = ("lwn","ravres") }
    //case "rc" => new PointerType { val uri = ("lwn","rc") }
    //case "zw" => new PointerType { val uri = ("lwn","zw") }
    //case "predkwa" => new PointerType { val uri = ("lwn","predkwa") }
    //case "#c" => new PointerType { val uri = ("lwn","Hc") }
    //case "#w" => new PointerType { val uri = ("lwn","Hw") }
    //case "predpo" => new PointerType { val uri = ("lwn","predpo") }
    case _ => {
      nonStandardRels += s
      new PointerType {
        val name = s
        val uri = ("lwn",s)
      }
    }
  }
}
