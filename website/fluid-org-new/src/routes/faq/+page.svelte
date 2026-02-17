<div id="grid" class="grid-container data-pane-hidden">
	<div></div>
	<div></div>

	<div class="flex-left-align">
		<h3 class="title">Frequently Asked Questions</h3>
		<p>
			<span class="faq-question">Why not do this for Python/R/Julia?</span> Fine-grained dependency
			tracking for Python or R would be difficult: both languages have significant libraries, such
			as
			<code>numpy</code> in Python, that depend heavily on C. Even if we could add
			dependency-tracking to Python, these libraries would remain opaque. Julia has less of a C
			dependency and could be a feasible target, although it would still probably require modifying
			the interpreter. Source-to-source transformations avoid some of these issues, but introduce
			other complications. An alternative direction we are exploring is the idea of evolving Fluid
			into a
			<a href="https://github.com/explorable-viz/fluid/issues/1139"
				>purely functional dialect of Python</a
			>
			and then provide our own version of libraries like
			<code>numpy</code>.
		</p>

		<p>
			<span class="faq-question">How mature is the language?</span>
			The language has a reasonable range of data types (integers, strings, floats, lists, tuples, matrices
			and dictionaries), as well as some convenient features like piecewise function definitions and list
			comprehensions. However, it lacks a
			<a href="https://github.com/explorable-viz/fluid/issues/571">module system</a>
			and support for basic IO, like
			<a href="https://github.com/explorable-viz/fluid/issues/117">loading CSV files</a>. Either of
			these would make a good student project!
		</p>

		<p>
			<span class="faq-question">How do I create a webpage containing a Fluid visualisation?</span>
			Unfortunately the authoring workflow is poor at the moment. Fluid is implemented in PureScript,
			and implementing a web page using Fluid currently requires a top-level PureScript
			<code>main</code>
			to specify additional program context, such as data sets and modules which need to be loaded along
			with the Fluid code. We plan to break this dependency on PureScript soon by providing a
			<a href="https://github.com/explorable-viz/fluid/issues/1057">command-line authoring tool</a>,
			which should streamline the authoring process considerably.
		</p>

		<p>
			<span class="faq-question">How does it work?</span> When you load a Fluid visualisation, the
			interpreter builds a <span class="highlight">dependence graph</span> capturing the dynamic IO
			dependencies of the program as it generates the visualisation. This graph is then used to add
			various affordances to the chart, so that selecting an element in the output can highlight the
			corresponding parts of the input, and vice versa. The relationship between the forward and
			backwards analyses over the graph is captured by the notion of a
			<span class="highlight">conjugate pair</span>, a pair of functions between Boolean algebras
			(sets of data elements) that are “near inverses” of each other in a particular way. (A
			conjugate pair is essentially another presentation of a
			<span class="highlight">Galois connection</span>; see our
			<a href="https://arxiv.org/abs/2403.04403">ESOP 2025 preprint</a> for more details.)
		</p>

		<p>
			<span class="faq-question">Why do images take a while to load?</span> Evaluating the program
			to a dependence graph can take a long time. We hope to be able to make some improvements to
			the performance of the interpreter soon, potentially by
			<a href="https://github.com/explorable-viz/fluid/issues/1008"
				>building the graph imperatively</a
			>, and are also considering ways to precalculate the dependence graph and bundle it with the
			visualisation. Finally, moving to a
			<a href="https://github.com/explorable-viz/fluid/issues/1162">Single-Page Application model</a
			> for websites built using Fluid will mean the figure won't be reloaded if you navigate back to
			the page.
		</p>

		<p>
			<span class="faq-question">Can I get involved as a student or intern?</span> If you are a
			student at University of Bristol, Charles University (Prague) or University of Cambridge you
			may be able to do a final-year or masters project on Fluid (see
			<a href="/student-projects">2024 projects</a>
			for details). In 2025 we will also have various summer internship schemes at the
			<a href="https://iccs.cam.ac.uk/">Institute of Computing for Climate Science</a>
			and <a href="https://www.cst.cam.ac.uk/">Department of Computer Science and Technology</a>,
			University of Cambridge. However these schemes tend to be in high demand and also have strict
			eligibility criteria.
		</p>

		<p>
			<span class="faq-question">How can I contribute as an open source developer?</span> We would
			especially welcome any contributions to the authoring framework, language features, or web
			publishing infrastructure. Please email
			<a href="mailto:roly.perera@dynamicaspects.org">Roly Perera</a> if there is anything you think you
			could help with, and we will add to you our Slack workspace and GitHub organisation.
		</p>
	</div>
</div>

<style>
	.faq-question {
		font-weight: 500;
		color: darkslategrey;
	}
</style>
