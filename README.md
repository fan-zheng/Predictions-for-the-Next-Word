---
title: "Predict the Next Word Project"
author: "Fan Zheng"
date: "April 21, 2017"
output: html_document
---


## Introduction

This is part of the Capstone course of Coursera Data Science specialization by Johns Hopkins University and Swiftkey company. The goal for this project is to build a Shiny application that will predict the next word according to user's input of words. Here I implement two algorithms, Kneser-Ney Smoothing and Katz Backoff. User input a sentence and the app will generate 5 option sorted from high to low probabilty to fill the next word. In this repository I provide the steps of processing the raw data, generating the training data and final look-up table and implementing the algorithms.


## Content

Train_test_dataset_preparation.html: Steps to process raw text data downloaded and generate trigrams, bigrams and unigrams data frame for further processing.
Dataset_Katzbackoff.R: Data preparation for Katzbackoff algorithm.
Dataset_Kneser_Ney.R: Data preparation for Kneser_Ney algorithm.
Function_Katzbackoff.R: Generate next word with Katzbackoff algorithm and how to test to find optimized discount number.
getnextwords.R: Implement Katz backoff and Kneser_Ney algorithm for the app
data_ngrams.Rdata: Kneser_Ney look-up dataset
data_ngrams_backoff.Rdata: Katz backoff dataset
ui.R
server.R

## Apps link:

https://fan-zheng.shinyapps.io/predictions_for_the_next_word/



