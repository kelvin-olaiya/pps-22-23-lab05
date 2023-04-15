package u05lab.ex2

import org.junit.Assert.assertEquals
import ConferenceReviewing.Question

class ConferenceReviewingTest:
  private var cr: ConferenceReviewing = ConferenceReviewing()

  @org.junit.Before
  def init(): Unit =
    this.cr = ConferenceReviewing()
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6

    val map: Map[Question, Int] = Map(
      Question.RELEVANCE -> 8,
      Question.SIGNIFICANCE -> 8,
      Question.CONFIDENCE -> 7,
      Question.FINAL -> 8
    )

    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @org.junit.Test
  def testOrderedScores(): Unit =
    assertEquals(List(4, 9), cr.orderedScores(2, Question.RELEVANCE))
    assertEquals(List(6, 7, 8), cr.orderedScores(4, Question.CONFIDENCE))
    assertEquals(List(10, 10), cr.orderedScores(5, Question.FINAL))

  @org.junit.Test
  def testAverageFinalScore(): Unit =
    // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)
    assertEquals(7.5, cr.averageFinalScore(2), 0.01)
    assertEquals(3.5, cr.averageFinalScore(3), 0.01)
    assertEquals(7.0, cr.averageFinalScore(4), 0.01)
    assertEquals(10.0, cr.averageFinalScore(5), 0.01)

  @org.junit.Test
  def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(Set(1, 2, 4), cr.acceptedArticles)

  @org.junit.Test
  def testSortedAcceptedArticles(): Unit =
    assertEquals(List(4 -> 7.0, 2 -> 7.5, 1 -> 8.5), cr.sortedAcceptedArticles)

  @org.junit.Test
  def optionalTestAverageWeightedFinalScore(): Unit =
    assertEquals((4.8 + 5.4) / 2, cr.averageWeightedFinalScoreMap(1), 0.01)
    assertEquals((9.0 + 6.0) / 2, cr.averageWeightedFinalScoreMap(2), 0.01)
    assertEquals((0.9 + 1.6) / 2, cr.averageWeightedFinalScoreMap(3), 0.01)
    assertEquals((3.6 + 5.6 + 5.6) / 3, cr.averageWeightedFinalScoreMap(4), 0.01)
    assertEquals((6.0 + 7.0) / 2, cr.averageWeightedFinalScoreMap(5), 0.01)
    assertEquals(5, cr.averageWeightedFinalScoreMap.size)
