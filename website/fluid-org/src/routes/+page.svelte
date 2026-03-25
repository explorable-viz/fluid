<script>
   import { CodeMirror, Grid } from '@explorable-viz/fluid';
</script>

<Grid>
	<div></div>
	<div></div>
	<div class="flex-left-align">
		<h3 class="title">Transparent research outputs</h3>
		<p>With traditional print media, the figures, text and other content are disconnected from the
		underlying data, making them hard to understand, evaluate and trust. Fluid is a programming language
		that tracks how data flows through a computation, making it possible to create computational outputs
		which are automatically linked to the underlying data.</p>

		<p>For some examples, see the <a
		href="https://github.com/explorable-viz/fluid-article">fluid-article</a> template repository.</p>

		<h3 class="title">Pure functional programming, Pythonic syntax</h3>
		<p>Fluid is based on <a href="https://github.com/pure-py/pure-py-spec">PurePy</a>,
		a pure (side-effect free) subset of Python. Fluid programs use familiar Python syntax
		&mdash; list comprehensions, dictionaries, lambdas &mdash; in a purely functional setting.</p>

		<div class="code-examples">
			<CodeMirror name="mergesort" src="fluid/mergesort.fld" open={true} />
			<CodeMirror name="moving-average" src="fluid/moving-average.fld" open={true} />
		</div>
	</div>
	<div></div>
</Grid>

<style>
	.code-examples {
		display: flex;
		flex-direction: column;
		gap: 1em;
	}
</style>
