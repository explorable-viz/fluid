<script lang="ts">
	import Grid from '$lib/components/Grid.svelte';
</script>

<Grid showDataPane="false">
	<div class="flex-left-align">
		<h3 class="title">Student Projects</h3>
		<p>
			Fluid is an exciting opportunity to work on new programming language foundations designed to
			make data science more open, intelligible and accessible. If you are:
		</p>
		<ul>
			<li>
				An undergraduate at a university other than Cambridge, able to work in the UK and looking
				for a paid summer research project
			</li>
			<li>
				A University of Cambridge MPhil or Part III student in Computer Science starting in October
				2025 and looking for a Masters project
			</li>
		</ul>
		<p>
			then we may have an opportunity for you; see the specific schemes below. Also check out our <a
				href="/fluid-poster.pdf">poster</a
			>.
		</p>

		<h4>Background</h4>
		<p>
			Charts and other visual summaries, curated by journalists and scientists from real-world data
			and simulations, are how we understand our changing world and the anthopogenic sources of that
			change. But the visual artifacts we are actually presented with are opaque: any relationship
			to the underlying data is lost. How can we expect to understand, critique or evaluate claims
			based on a bitmap? This is challenging enough for an expert with access to the source code and
			data used to derive the outputs; for a non-expert the prospects are even worse.
		</p>

		<p>
			<a href="https://f.luid.org">Fluid</a> is a new “transparent” programming language, being developed
			at the Institute of Computing for Climate Science in Cambridge in collaboration with University
			of Bristol, that makes it easy to create charts and figures which are linked to data, enabling a
			user to interactively discover what visual elements actually represent. The key idea is to incorporating
			a bidirectional dynamic dependency analysis into the language runtime, allowing it to track dependencies
			that arise as as outputs (such as charts and tables) are computed from data. This information is
			then used to automatically enrich rendered output with interactions which allow a reader to explore
			the relationship to data directly through the artefact, by selecting visual features of interest.
			Fluid uses so-called “program slicing” techniques based on Galois connections, a neat mathematical
			abstraction which characterises exactly the relationship between sets of inputs and sets of outputs
			which depend on them.
		</p>

		<p>
			A key use case for Fluid is to make it easy to present real-world climate science in
			“long-form” explorable essays and interactive articles intended for non-specialist audiences,
			such as policymakers. (See <a href="https://distill.pub">distill.pub</a> for some examples of so-called
			“explorable explanations”.)
		</p>

		<h4>Undergraduate research internships 2025</h4>

		<p>
			We have up to 4 summer internship positions available, funded through the following schemes,
			with at least one more position opening later in the Spring. These are all <strong
				>widening participation</strong
			>
			internships, meaning you <strong>must</strong> meet at least one of the socio-economic or underrepresentation
			criteria listed on the web page for the particular scheme:
		</p>

		<ul>
			<li>
				<a href="https://www.cst.cam.ac.uk/outreach/deepmind-research-ready"
					>Google DeepMind Research Ready</a
				> programme (deadline 09:00 Mon 17 February 2025).
			</li>
			<li>
				<a href="https://www.epsrc.group.cam.ac.uk/DTP/internships">STEM Widening Participation</a>
				programme (deadline Friday 14 March 2025).
			</li>
		</ul>

		<p>
			You must also be in your penultimate or final year of your undergraduate degree and on course
			for a 1st or 2:1.
		</p>

		<p>
			Your internship project will live somewhere at the intersection of programming languages
			research, AI/ML and HCI, depending on your skills/background and research interests. There are
			two main goals driving the next iteration of Fluid; both involve using (black-box) LLMs to
			achieve goals relating to transparency and open science.
		</p>

		<p>
			A) <strong>Extending Fluid with computational explanations:</strong> information about the
			specific steps that were involved in computing a particular feature of the output (e.g. the
			whiskers decorating a bar in a bar chart). This is a potentially powerful transparency
			feature, allowing readers (perhaps during peer review) to discover otherwise hidden or
			obscured facts about the data underpinning a visualisation. One internship project will
			involve turning these computational explanations into more user-friendly natural language
			explanations that would be useful for lay readers as well as expert readers. The novel
			contribution that Fluid can make to this problem is to provide an authoritative ground truth
			for the generation of the natural language, offering the prospect of a “trusted” or reliable
			form of open, self-explanatory artifact. (See “AI reading assistant” in the
			<a href="/fluid-poster.pdf">poster</a>.)
		</p>

		<p>
			B) <strong>Extending Fluid with transparent text:</strong> natural language (such as the
			expository text in a climate report for policymakers) which is underwritten by a semi-formal
			computational interpretation, especially quantitative phrases or other fragments of text
			expressing data-driven claims. For example, the statement that under a particular emissions
			scenario, global warming is
			<i>extremely likely</i> to exceed 2°C in the 21st century can be underwritten by a Fluid
			program that assigns a specific interpretation to this text in terms of the distributions of
			the underlying data used (by the report author) to reach that conclusion. Another internship
			project will be to develop AI tooling which replaces fragments of text by expressions that
			compute that text from data. (See “AI authoring assistant” in the
			<a href="/fluid-poster.pdf">poster</a>.)
		</p>

		<h4>Cambridge MPhil/Part III students: 2025-2026 Projects</h4>

		<p>
			University of Cambridge MPhil or Part III students starting October 2025 may be interested in
			the following opportunity.
		</p>

		<p>
			The live demos on the <a href="https://f.luid.org/">Fluid website</a> only scratch the surface of
			what transparent programming languages like Fluid should enable when fully realised. There are many
			opportunities for an imaginative and technically strong student to help move this idea forward as
			part of an MPhil project. Your research could go in a number of directions, depending on whether
			your interests lie more towards programming languages, AI/ML or HCI. A programming languages project
			would extend Fluid into a literate programming tool, by adding Markdown support and the ability
			to embed computational content via a Lisp-style backquote mechanism. A more mathematical project
			might add multidimensional arrays to the language, along with various array operations inspired
			by linear algebra and an extension of the dependency analysis to these new operations. A project
			focused on AI/ML might focus on generating natural language explanations from provenance traces,
			or interpreting fragments of natural language as having a formal underpinning in an evidence base.
		</p>

		<p>
			If you think any of this sounds interesting, please get in touch with <a
				href="mailto:roly.perera@cl.cam.ac.uk">Dr Roly Perera</a
			>, Early Career Advanced Fellow, Institute of Computing for Climate Science, University of
			Cambridge to arrange an initial chat. Whatever form your project takes, we would aim for your
			work to be incorporated into our main development codebase, and so would form a genuine
			contribution to the overarching project. You will get to present your work to researchers and
			data scientists at the Institute of Computing for Climate Science and The Alan Turing
			Institute, and work with PhD students at Cambridge and Bristol. A strong background in
			functional programming, maths and/or science is a must. You can expect to gain experience in
			programming languages research, data analysis and data visualisation, with close supervisor
			support.
		</p>
	</div>
</Grid>
