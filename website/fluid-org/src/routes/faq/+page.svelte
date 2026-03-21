<script>
    import { Grid } from '@explorable-viz/fluid';
</script>

<Grid showDataPane="false">
	<div></div>
	<div></div>

	<div class="flex-left-align">
		<h3 class="title">Frequently Asked Questions</h3>
		<p>
			<span class="faq-question">How does Fluid work?</span> When you load a Fluid visualisation, the
			interpreter builds a <span class="highlight">dependence graph</span> capturing how data flows through
			the program. This graph is used to add various interactions to the chart (so that, for example,
			selecting an element in the output highlights the corresponding parts of the input). See our
			<a href="https://arxiv.org/abs/2403.04403">ESOP 2025 preprint</a> for details.
		</p>

		<p>
			<span class="faq-question">Why not do this for Python/R/Julia?</span> Fine-grained dependency
			tracking for Python or R would be difficult: both languages have key libraries that depend heavily on
			C. Julia has less of a C dependency and could be a feasible target, although it would still probably
			require modifying the interpreter. Our approach is to develop a pure functional dialect of Python,
			for which we can then provide our own version of libraries like
			<code>numpy</code>.
		</p>

		<p>
			<span class="faq-question">What's next for the language?</span>
			Most likely a gradual type system, better tooling and performance improvements.
		</p>
	</div>
</Grid>

<style>
	.faq-question {
		font-weight: 500;
		color: darkslategrey;
	}
</style>
