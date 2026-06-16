<script>
   import { CodeMirror, Grid } from '@fluid-org/fluid';
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
		href="https://github.com/fluid-org/fluid-article">fluid-article</a> template repository.</p>

		<h3 class="title">Pure functional programming, Pythonic syntax</h3>
		<p>Fluid is based on <a href="https://github.com/pure-py/pure-py-spec">PurePy</a>,
		a pure (side-effect free) subset of Python. Fluid programs use familiar Python syntax
		(list comprehensions, dictionaries, lambdas) in a purely functional setting.</p>

		<div class="code-examples">
			<h4>Variable definitions</h4>
			<CodeMirror name="variable-definitions" src="fluid/variable_definitions.fld" details={false} />

			<h4>Pattern matching and destructuring</h4>
			<CodeMirror name="pattern-matching" src="fluid/pattern_matching.fld" details={false} />

			<h4>List comprehensions and lambdas</h4>
			<CodeMirror name="list-comprehensions" src="fluid/list_comprehensions.fld" details={false} />

			<h4>Partial application</h4>
			<CodeMirror name="partial-application" src="fluid/partial_application.fld" details={false} />

			<h4>Match expressions</h4>
			<CodeMirror name="match-expressions" src="fluid/match_expressions.fld" details={false} />
		</div>
	</div>
	<div></div>
</Grid>

<style>
	.code-examples {
		display: flex;
		flex-direction: column;
		gap: 0.5em;
	}

	h4 {
		margin: 0.5em 0 0 0;
		font-size: 10pt;
	}
</style>
