package ex2.test

import ex2.ConferenceReviewing
import ex2.Question
import org.junit.Test

class ReviewTest:
    val confReview = ConferenceReviewing()

    @Test def testLoadReview(): Unit = 
        val article = 0
        val scores = Map(Question.RELEVANCE -> 1)
        confReview.loadReview(article = article, scores = scores) 
    
