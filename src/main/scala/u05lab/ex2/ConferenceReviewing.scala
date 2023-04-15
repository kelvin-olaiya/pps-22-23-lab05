package u05lab.ex2

trait ConferenceReviewing:
  import ConferenceReviewing.Question

  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl() extends ConferenceReviewing:
    private var reviews: List[(Int, Map[Question, Int])] = List()

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      reviews = reviews :+ (article, scores)

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      loadReview(article, Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin
      ))

    override def orderedScores(article: Int, question: Question): List[Int] =
      scores(article, question).sorted

    override def averageFinalScore(article: Int): Double =
      scores(article, Question.FINAL).sum / countReviews(article).toDouble

    private def scores(article: Int, question: Question): List[Int] =
      reviews.collect({ case (a, m) if a == article => m(question) })

    private def countReviews(article: Int) = reviews.count(_._1 == article)

    override def acceptedArticles: Set[Int] = reviews.map(_._1).filter(isAccepted).toSet

    private def isAccepted(article: Int) =
      averageFinalScore(article) > 5 && reviews.exists((a, m) => a == article && m(Question.SIGNIFICANCE) >= 8)

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.map(a => (a, averageFinalScore(a))).toList.sortBy(_._2)

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.groupMapReduce(_._1)(r =>
        r._2(Question.CONFIDENCE) * r._2(Question.FINAL) / (10.0 * countReviews(r._1))
      )(_ + _)
