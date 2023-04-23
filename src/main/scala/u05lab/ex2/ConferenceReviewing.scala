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
      reviews.collect({ case (article, scores) if article == article => scores(question) })

    private def countReviews(article: Int) = reviews.count(_._1 == article)

    override def acceptedArticles: Set[Int] = reviews.map(_._1).filter(isAccepted).toSet

    private def isAccepted(article: Int) = {
      val acceptanceMinimumAverageScore = 5
      val acceptanceArticleSignificanceScore = 8
      averageFinalScore(article) > acceptanceMinimumAverageScore
        && reviews.exists((article, scores) =>
        article == article && scores(Question.SIGNIFICANCE) >= acceptanceArticleSignificanceScore
      )
    }

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.map(article => (article, averageFinalScore(article))).toList.sortBy(_._2)

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.groupMapReduce(_._1)(review => {
        val maximumScore = 10.0
        review._2(Question.CONFIDENCE) * review._2(Question.FINAL) / (maximumScore * countReviews(review._1))
      }
      )(_ + _)
