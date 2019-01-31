# Statistical Machine Learning

This is the Github repo for the course Statistical Machine Learning offered at Columbia University. I set this up for future students and hope this site can be helpful. Please feel free to email me if you have questions. 

# Advice

I do not believe there is one book or one problem set to do so that one can magically become an expert in machine learning. That being said, there are a few directions to go so that perhaps you can be on the right track. On top of that, your dilligence is a great contributing factor to determine how far you can push yourself in this field. 

(1) Read as many books as you can and try to replicate the machine learning techniques. This is early stage of getting yourself familiar with machine learning tools and you should feel comfortable of getting your hands dirty. 

Some great books are:
- An Introduction to Statistical Learning by Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani; click [here](https://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf)
- An Introduction to Support Vector Machines and Other Kernel-based Learning Methods by Cristianini; click [here](https://www.amazon.com/Introduction-Support-Machines-Kernel-based-Learning/dp/0521780195)
- Elements of Statistical Learning by Trevor Hastie, Robert Tibshirani, Jerome Friedman; click [here](https://web.stanford.edu/~hastie/Papers/ESLII.pdf)
- Machine Learning by Kevin Murphy; click [here](https://www.cs.ubc.ca/~murphyk/MLbook/)

Programming Languages:
- You should be fluent in both Python and R. 
- You should attempt to replicate same results in C++. 

A good exercise is to go to Online Code Compiler, click [here](https://rextester.com/l/r_online_compiler) and do some matrix algebra with different languages simultaneously. 

(2) For intermediate level students, you should be fluent in Step (1). To move beyond here, you need to go to Github or Kaggle and search for new data sets (the ones you have never touched before) and try to replicate Step (1) using new data sets. 

Review this [Wiki Site](https://en.wikipedia.org/wiki/List_of_datasets_for_machine_learning_research) and Search for New Data Set. Once you find something interesting you can go to Github or Kaggle.

A new data set is like a new person you may want to be friends with. You treat it well and learn from it. You will gain experience. The data set does not limit to any form. It can be (1) big of small, (2) supervised or unsupervised, (3) time-series, (4) images, and so on. you need to be able to tell a great story with results from multiple different machine learning techniques given any data sets. 

(3) At an advanced or research level, you are fluent in Step (1) and (2). In fact, you might be too fluent to find them interesting. Moreover, you have looked so many data sets that there isn't a single data form you have not seen before. You start to think how you can contribute to the society and what can be improved. You start asking questions such as "why apples fall?" If you are here, you are an advanced machine learning practitioner. You can override any authors or textbooks. You can design and even invent profitable machine learning products so that perhaps you can go out there to look for investors to finance you idea and start your own company. 

# Example of Machine Learning Story

**Brief** To become a good machine learning practitioner, simply finishing school work is not enough. I recommend you find a topic you truly love and try to adopt theoretical framework to that topic. 

Take myself for an examle, I love looking at stock charts. A lot of theoretical formula I can relate them to stock market. 

First, I started by looking at correlation. This links to running regression and I have conducted experiment to learn that myself when I was a freshman. What is the linear relationship between market return and a stock return? How about fundamental values? Paper is [here](https://yiqiaoyin.files.wordpress.com/2016/08/how-to-understand-future-returns-of-a-securityef80a5-revised-2014.pdf).

Next, I started to size up. A data composed of stock-to-market comparison seemed too easy, so I started looking at cross-sectional finance data. The common data set that is used for PhD students in finance is CRSP stock universe. This is where I started to work with large-scale matrices that are 2GB in size and when I learned to work with different forms of data frames. It started to turn into tedious data clean-up work. However, the results are interesting. Paper is [here](https://yiqiaoyin.files.wordpress.com/2016/08/cross-section-study-on-stock-returns-to-future-expectation-theorem.pdf) and this [one](https://yiqiaoyin.files.wordpress.com/2016/08/alternative-empirical-study-on-market-value-balance-sheet.pdf) is interesting too.

After looking at tons of stock returns data set, I got bored at conventional way of analyzing stock market. I invented this term "greed" so that I can turn this into a supervised model for myself to study. This is not publishable idea, but quite interesting to me. Paper is [here](https://yiqiaoyin.files.wordpress.com/2016/05/empirical-study-on-greed.pdf). 

In 2016 I took a little diregression to study macro-economics. I did a project on contact rate in game theory. The paper was too hard, but the coding part is easy. I got stuck on dynamic model in macro material and it took me 3 months to figure this out. It lead to nowhere, but I am grateful to the professor who instructed me and him spending time with me on this project. Paper is [here](https://yiqiaoyin.files.wordpress.com/2016/10/trade-dynamics-with-endogenous-contact-rate.pdf). 

After trading floor, I took a little bit of time and summarizing my experiences on the trading floor and they become the following series of papers:
- [Buy Signal from Limit Theorem](https://yiqiaoyin.files.wordpress.com/2018/05/buy-signal-from-limit-theorem.pdf)
- [Time Series Analysis on Stock Returns](https://yiqiaoyin.files.wordpress.com/2017/05/time-series-analysis-on-stock-returns.pdf)
- [Martingale to Optimal Trading](https://yiqiaoyin.files.wordpress.com/2016/10/martingale-to-optimal-trading.pdf)
- [Anomaly Correction by Trading Frequency](https://yiqiaoyin.files.wordpress.com/2016/09/anomaly-correction-by-trading-frequency.pdf)
- [Absolute Alpha with Moving Averages: A Consistent Trading Strategy](https://yiqiaoyin.files.wordpress.com/2016/05/absolute-alpha-with-moving-averages.pdf)
- [Absolute Alpha with Limited Leverage](https://yiqiaoyin.files.wordpress.com/2016/05/absolute-alpha-with-limited-leverage.pdf)
- [Absolute Alpha by Beta Manipulation](https://yiqiaoyin.files.wordpress.com/2016/05/absolute-alpha-by-beta-manipulation.pdf)
and this when I started to build up intra-discplinary experience across the fields of (1) trading, (2) probability theory, (3) accounting, and (4) game theory. Most of what I am right now depend on these past couple of papers.

Interesting phenomenon (and you probably saw it too) was that I did not have any machine learning technique discussed yet. However, the few papers above gave me a unique view so that I can create any stock data however I want. This is when machine learning comes in, which leads to this paper: [Robust Portfolio by Influence Measure with presentation and Github](https://yiqiaoyin.files.wordpress.com/2018/12/rubust-portfolio-by-influence-measure-yiqiao-yin-2018.pdf)

I am now equipped with two unique skills: (1) I can use probability theory to manipulate and invent any stock data helpful for me; and (2) I can invent a machine learning algorithm that solves that problem. All of these ideas are updated frequently [here](https://yinscapital.com/private-collection/)
