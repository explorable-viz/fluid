<script>
   import { Grid } from '@explorable-viz/fluid';
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

<div class="code-block"><pre>def mergesort(xs):
  if length(xs) &lt; 2: xs
  else:
    def (ys, zs): split(xs)
    merge(mergesort(ys), mergesort(zs))</pre></div>

<div class="code-block"><pre>def movingAvg(ys, window):
  [sum([nthPad(n, ys)
        for n in [i - window .. i + window]])
     / (1 + 2 * window)
   for i in [0 .. length(ys) - 1]]</pre></div>

	</div>
	<div></div>
</Grid>

<style>
	.code-block {
		background-color: #1e1e1e;
		border-radius: 8px;
		padding: 1em 1.2em;
		margin: 1em 0;
		overflow-x: auto;
	}

	.code-block pre {
		margin: 0;
		color: #d4d4d4;
		font-family: 'SF Mono', 'Fira Code', 'Consolas', 'Monaco', monospace;
		font-size: 13px;
		line-height: 1.5;
		white-space: pre;
	}
</style>
