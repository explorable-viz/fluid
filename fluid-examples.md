# Fluid Examples

Working name for project, with repo `fluid-examples`. Primarily Achintya and Roly with possibly some involvement from Joe Bond (PhD student at Bristol) and Dominic Orchard (ICCS Co-Director and Lecturer at Kent). From July we will probably have 1-2 interns.

Over the project Roly and Joe will also be working on a new design of the core dependency-tracking approach but this is unlikely to make it into this iteration of Fluid Examples.

## Expected outputs

3 mini articles hosted on the web, each with 2-3 interactive visualisations written in Fluid. Based on existing research and chosen strategically to develop relationships/collaborations. Published with DOIs and perhaps using something like the Distill template (distill.pub).

Some ideas:
- VESRI (Virtual Earth System Research Institute) paper
- Environmental Data Science or other Cambridge University Press publication
- Some work affiliated to Cabot Institute for the Environment, University of Bristol

It would be nice if the mini-articles also illustrated something that has a "reveal" when you explore the provenance of the data. For example BBC articles about "clean" hydroelectric power being a significant source of methane emissions, and "clean" power stations burning wood chips sourced from pristine rainforest in British Colombia. Of course these aren't "reveals" that our infrastructure is able to magically bring about -- that takes data journalism -- but it should be able to make it easier to present this kind of information in a way that is "revealing" (maybe).

## Timeframe

- 6 months initially (would be nice to extend but will depend on funding).
- Roly busy for first 3 months with POPL paper (deadline 10 July) and two presentations (Bristol Digital Futures Institute workshop, 10 May and ICCS Summer School, 10 July).
- From around 10 July we should have 2 students (Chau Crawford and Haofei Chen, both University of Edinburgh) as residential interns at Cambridge. I'm currenty planning for both of these internships to involve implementing new Fluid features to improve the 3 mini-articles and tooling around them.

Given the above I propose we split the project into roughly 2 halves, as outlined below.

### Phase 1: Article selection/development (~3 months)

From project start until 10 July or thereabouts I propose we focus mainly on selecting target research papers, identifying content we would like to re-present using Fluid, and making an initial pass over some of the text and visualisations. Roly will be making improvements to Fluid performance and visual interface over this period to support the BDFI and ICCS Summer School presentations, so these can also feed into this phase of Fluid Examples. Hopefully Achintya can attend one day of the ICCS Summer school and present some of the initial work along with Roly and Joe.

Achintya can also use this time to identify key tooling requirements and other infrastructure tasks that will make it easier for third-parties to experiment with or contribute to Fluid, although it's unlikely we'll have much time to work on this before July.

### Phase 2: Improvements to article authoring support (~3 months)

From 10 July or thereabouts onwards, Roly will be substantially freed up and working closely with the interns for 2 months. I think we can aim to use the internships to substantially streamline the content authoring experience, e.g. by adding Markdown support. Any remaining time can be spent on UI/visualisation improvements to improve the experience for readers, and tweaking the written and visual content to get it into a form that we can publish at a persistent DOI.

## Research paper

After the project completes we would also like to use the 3 mini-articles as case studies in a research paper or position paper about transparent data visualisations. We could go for something like IEEE Viz (maybe with Benjamin Bach as a coauthor) or Nature Computational Science.
