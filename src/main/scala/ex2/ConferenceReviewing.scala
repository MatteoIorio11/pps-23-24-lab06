package ex2

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

    override def averageFinalScore(article: Int): Double = ???

    override def averageWeightedFinalScoreMap: Map[Int, Double] = ???

    override def sortedAcceptedArticles: List[(Int, Double)] = ???

    override def acceptedArticles: Set[Int] = ???

    override def orderedScores(article: Int, question: Question): List[Int] = ???

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = ???

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit = ???

    private val scores = Map[Int, Map[Question, Int]]()



