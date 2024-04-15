package ex2.test

import ex2.ConferenceReviewing
import ex2.Question
import org.junit.Test

class ReviewTest:
    private val confReview = ConferenceReviewing()

    @Test def testLoadReview(): Unit = 
        val article = 0
        val scores = Map(Question.RELEVANCE -> 1)
        confReview.loadReview(article = article, scores = scores) 
    @Test def testLoadReviewWithParameters(): Unit =
        val article = 0
        val relevance = 1
        val significance = 1
        val confidence = 1
        val fin = 1
        confReview.loadReview(article, relevance, significance, confidence, fin)

