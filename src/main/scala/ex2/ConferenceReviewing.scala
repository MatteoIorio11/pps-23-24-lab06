package ex2

import scala.compiletime.ops.int
import scala.collection.mutable.ArrayBuffer

enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

trait Reviewing:
    def loadReview(article: Int, scores: Map[Question, Int]): Unit
    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
    def orderedScores(article: Int, question: Question): List[Int]
    def averageFinalScore(article: Int): Double
    def acceptedArticles: Set[Int]
    def sortedAcceptedArticles: List[(Int, Double)]
    def averageWeightedFinalScoreMap: Map[Int, Double]
class ConferenceReviewing extends Reviewing:
    private var internalScores = scala.collection.mutable.Map[Int, Map[Question, scala.collection.mutable.ArrayBuffer[Int]]]()

    override def averageFinalScore(article: Int): Double = 
        require(article >= 0)
        val optScores = internalScores.get(article)
        0


    override def averageWeightedFinalScoreMap: Map[Int, Double] = ???

    override def sortedAcceptedArticles: List[(Int, Double)] = ???

    override def acceptedArticles: Set[Int] = ???

    override def orderedScores(article: Int, question: Question): List[Int] =
        require(article >= 0)
        require(question != null)
        val optMap = internalScores.get(article)
        if optMap.isDefined then
            val optQuestionMap = optMap.get.get(question)
            if optQuestionMap.isDefined then
                return optQuestionMap.get.sortWith((x, y) => x < y).toList
        List()


    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = 
        var scores = Map[Question, Int]()
        require(relevance >= 0 && relevance <= 10)
        require(significance >= 0 && significance <= 10)
        require(confidence >= 0 && confidence <= 10)
        require(fin >= 0 && fin <= 10)
        scores += (Question.RELEVANCE -> relevance)
        scores += (Question.SIGNIFICANCE -> significance)
        scores += (Question.CONFIDENCE -> confidence)
        scores += (Question.FINAL -> fin)
        loadReview(article = article, scores = scores)

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
        require(scores != null)
        require(article >= 0)
        if internalScores.isDefinedAt(article) then
            internalScores(article)
              .foreachEntry((question, score) =>
                  scores.foreachEntry((key, value) => if question == key then score ++= ArrayBuffer(value)))
        else
            val newMap = scores.map((k, value) => (k, ArrayBuffer(value)))
            internalScores += (article -> newMap)

