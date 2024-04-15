package ex2.test

import ex2.ConferenceReviewing
import ex2.Question
import org.junit.Test
import org.junit.Assert.assertEquals
class ReviewTest:
    private val cr = ConferenceReviewing()

    @org.junit.Before
    def init(): Unit
        cr.loadReview(1, 8, 8, 6, 8);
        cr.loadReview(1, 9, 9, 6, 9); // 5.4
        cr.loadReview(2, 9, 9, 10, 9); // 9.0
        cr.loadReview(2, 4, 6, 10, 6); // 6.0
        cr.loadReview(3, 3, 3, 3, 3); // 0.9
        cr.loadReview(3, 4, 4, 4, 4); // 1.6
        cr.loadReview(4, 6, 6, 6, 6); // 3.6
        cr.loadReview(4, 7, 7, 8, 7); // 5.6

    @Test def testOrderedScores(): Unit =
        assertEquals(cr.orderedScores(2, Question.RELEVANCE), List(4, 9));
        assertEquals(cr.orderedScores(4, Question.CONFIDENCE), List(6, 7, 8));
        assertEquals(cr.orderedScores(5, Question.FINAL), List(10, 10));
    @Test def testAverageFinalScore(): Unit = ???
        