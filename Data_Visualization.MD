# Small Note of Data Visualization

## Brief

This is a write-up for [Professor Robbins](https://github.com/jtr13) about my findings of related topics and what can potentially be implemented for Stats Graphics course at Columbia Universit

## Background

Most of the information in industry are passed through by visualization. To efficiently carry out successful visualization, the presenter needs to understand (1) audience, (2) goals, and (3) techniques. 

## Acknowledgement

The content of this article focus on things that may not appear in conventional graphical design. Techniques such as bar plots, boxplots, error bars, and etc. are of course very important. To prevent ourselves being redundant, this article chooses to discuss the materials that is outside the scope of conventional graphical design. Some of the ideas in this article may be totally out of the range such as using graphical software implemented for biotech firms. These ideas are of references only and are discussed as background knowledge. 

## Content

Audience is placed at the first. An understanding of the audience group is crucial for graph designers because every person has his or her unique background and may see things from a special aspect. Just like computer software that need to be personalized, a good graphical presentation should adapt to audience body as well. For example, there is a saying "If you are dealing with inexperienced clients, stay away from advanced solutions. But if you are meeting with highly skilled professionals, going beyond pies and charts is mandatory". 

Goals can be from the presenter's point of view but will be much better if they are also from audience's point of view. This is important for presenter and allows presenter to carry out a central message. On the other hand, the goal should be something helpful for the audience as well. To illustrate this idea, imagine a spectrum with one end to be a central message completely ordinary (i.e. 1+1=2) and the other end a never-ending math proof (that looks like *jd12&@#... and lasts 5 pages). Well, you want to be in the middle. You want to express something elegant such as Newton's 2nd Law F = ma. There is always a sweet spot in between to illustrate a novel idea but in an approach that most audience can understand. This is, however, not easy to do which is what visualization comes in. 

The following contents I will start with industry of Consulting, Finance/Fintech, and Biotech as case studies. Each case study I will list out some of the links and websites I have found that could be helpful for visualization according to each industry. I will end this article with some additional techniques I found useful but less emphasized before.  

## INDUSTRY: Consulting - [1] Case Study: Mckinsey

If you are at Mckinsey, what are the types of articles and reports people read? And henceforth, if you present to people at Mckinsey, what are some of the graphs you need to be able to do?
- [AI for Social Good](https://www.mckinsey.com/featured-insights/artificial-intelligence/applying-artificial-intelligence-for-social-good): This is an article written by Mckinsey for open public. The key visualization in this article is the Exhibit 1, which is composed of 10 circles representing the number of cases per domain.
- [Visualization the use of AI](https://www.mckinsey.com/featured-insights/artificial-intelligence/visualizing-the-uses-and-potential-impact-of-ai-and-other-analytics): This is an article on visualization and the interactive component of this article actually used Tableau which seem to be the hot software in industry right now.
- [Women in Workplace 2018](https://www.mckinsey.com/featured-insights/gender-equality/women-in-the-workplace-2018): The graph of this article is a classic example of what I found challenging. I know how to create each component of the graph, but in the middle the area of shaded region changed. Subtle details like this are almost always easier said than done. 
- [Technical Reads from Mckinsey](https://medium.com/digital-mckinsey/technical-reads-from-mckinsey-digital-labs-experts-d04a48aeaf4): Not necessary as textbooks, but good resources to keep in library.
- [Mckinsey Report "Age of Analytics"](https://www.mckinsey.com/~/media/McKinsey/Business%20Functions/McKinsey%20Analytics/Our%20Insights/The%20age%20of%20analytics%20Competing%20in%20a%20data%20driven%20world/MGI-The-Age-of-Analytics-Full-report.ashx): This is a full report (once a year, I assume) about insights Mckinsey analysts love to share. Page 14 Exhibit E6 has a very interesting visualization composed of circles (again, they love circles). I attached a screenshot below; this could be coded in D3, but I felt like there should be a template for doing something like this and that'll be really helpful.

<p align="center">
  <img width="500" src="https://github.com/yiqiao-yin/Statistical-Machine-Learning/blob/master/figs/data-viz-note-fig-1.png">
</p>

## INDUSTRY: Finance/Fintech - [2] Case Study: AQR Capital

AQR Capital is a global money management firm. It manages capital for investors all around the world and they charge expensive management fees (much more expensive than typical 401K plan offered by State Street for example). This is sort of "failed" case in data visualization because of complex finance data set. From my experience, cross section time-series data set is very difficult to visualize. As of today, information in this field is still passed by regression tables such as coefficients and standard errors (basically numbers). 
- [Data Visualization Developer](https://www.linkedin.com/jobs/view/data-visualization-developer-at-aqr-capital-management-393681534/): this is a job post by AQR on LinkedIn; in the description of this position, we can see things like SQL, experience in building APIs, experience with Tableau or any other BI tools, experience in Agile environment and so on; a job description such as this automatically puts data science and statistics students at a lower deck of profiles than computer science students when screening candidates; in other words, if I were the employer and I just want someone to draw me a graph on computer, I will probably prefer computer science background people simply because I don't know statisticians can perform this task. If I were told by schools or candidates with stats background that they are also able to perform this task, I would be more informed when hiring candidates.  
- [Data Visualization by a third party](http://www.jenn-o.com/test) hired by AQR: this is a third party hired by AQR, they are independent art designs and they made videos on complex finance data provided by AQR; we do not have to admire everything we see; sometimes it might be interesting for students to hear about criticism; the graph in the video is not very sophisticated and contained mislabel as well as unclear notations (screen shot below).
- Data Visualization from existing paper, ex: [Betting against Beta](https://www.sciencedirect.com/science/article/pii/S0304405X13002675#t0015): this is a paper published in Journal of Financial Economics (JFE) and jointly with AQR because AQR provided the data sets that the authors used. In Table 3, they presented a cross-section regression table. An interesting project can simply be convert this table into graphs. That being said, it is not to force all tables into graphs. Some information may make more sense in tables. In other words, better questions can be (1) how to visualize a regression model? (2) how to visualize comparison of multiple regression model? (3) say X1 has high t-value in a regression model, how do visualize that?
- [AQR on Trend Not Being Bad, Yet](http://rivershedge.blogspot.com/2019/05/aqr-on-trend-not-being-dead-yet.html): This is a blog is trying to comment on an [article](https://images.aqr.com/-/media/AQR/Documents/Whitepapers/You-Cant-Always-Trend-When-You-Want.pdf) published by AQR arguing that trading strategies following trend have been going downhill. To conclude the take-away from AQR's paper, this blogger designed a graph (screen shot below). I like this graph a lot but it is very difficult to read. An interesting project can be attempting to convert this graph into a time-series or interactive app.


