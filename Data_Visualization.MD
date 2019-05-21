# Small Note of Data Visualization

## Brief

This is a write-up for [Professor Robbins](https://github.com/jtr13) about my findings of related topics and what can potentially be implemented for Stats Graphics course at Columbia Universit

## Background

Most of the information in industry are passed through by visualization. To efficiently carry out successful visualization, the presenter needs to understand (1) audience, (2) goals, and (3) techniques. 

## Acknowledgement

The content of this article focus on things that may not appear in conventional graphical design. Techniques such as bar plots, boxplots, error bars, and etc. are of course very important. To prevent ourselves being redundant, this article chooses to discuss the materials that is outside the scope of conventional graphical design. Some of the ideas in this article may be totally out of the range such as using graphical software implemented for biotech firms. These ideas are of references only and are discussed as background knowledge. 

# Content:

Audience is placed at the first place. An understanding of the audience group is crucial for graph designers because every person has his or her unique background and may see things from a special aspect. Just like computer software that need to be personalized, a good graphical presentation should adapt to audience body as well. For example, there is a saying "If you are dealing with inexperienced clients, stay away from advanced solutions. But if you are meeting with highly skilled professionals, going beyond pies and charts is mandatory". 

## INDUSTRY: Consulting - [1] Case Study: Mckinsey

If you are at Mckinsey, what are the types of articles and reports people read? And henceforth, if you present to people at Mckinsey, what are some of the graphs you need to be able to do?
- [AI for Social Good](https://www.mckinsey.com/featured-insights/artificial-intelligence/applying-artificial-intelligence-for-social-good): This is an article written by Mckinsey for open public. The key visualization in this article is the Exhibit 1, which is composed of 10 circles representing the number of cases per domain.
- [Visualization the use of AI](https://www.mckinsey.com/featured-insights/artificial-intelligence/visualizing-the-uses-and-potential-impact-of-ai-and-other-analytics): This is an article on visualization and the interactive component of this article actually used Tableau which seem to be the hot software in industry right now.
- [Women in Workplace 2018](https://www.mckinsey.com/featured-insights/gender-equality/women-in-the-workplace-2018): The graph of this article is a classic example of what I found challenging. I know how to create each component of the graph, but in the middle the area of shaded region changed. Subtle details like this are almost always easier said than done. 
- [Technical Reads from Mckinsey](https://medium.com/digital-mckinsey/technical-reads-from-mckinsey-digital-labs-experts-d04a48aeaf4): Not necessary as textbooks, but good resources to keep in library.
- [Mckinsey Report "Age of Analytics"](https://www.mckinsey.com/~/media/McKinsey/Business%20Functions/McKinsey%20Analytics/Our%20Insights/The%20age%20of%20analytics%20Competing%20in%20a%20data%20driven%20world/MGI-The-Age-of-Analytics-Full-report.ashx): This is a full report (once a year, I assume) about insights Mckinsey analysts love to share. Page 14 Exhibit E6 has a very interesting visualization composed of circles (again, they love circles). I attached a screenshot below; this could be coded in D3, but I felt like there should be a template for doing something like this and that'll be really helpful.

<p align="center">
  <img width="800" src="https://github.com/yiqiao-yin/Statistical-Machine-Learning/blob/master/figs/data-viz-note-fig-1.png">
</p>

