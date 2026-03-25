<script>
   import { Grid } from '@explorable-viz/fluid';
   import { onMount } from 'svelte';
   import hljs from 'highlight.js/lib/core';
   import python from 'highlight.js/lib/languages/python';
   import 'highlight.js/styles/github-dark.css';

   hljs.registerLanguage('python', python);

   onMount(() => {
      document.querySelectorAll('pre code').forEach(el => hljs.highlightElement(el));
   });
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

<div class="code-block"><pre><code class="language-python">def mergesort(xs):
  if length(xs) &lt; 2: xs
  else:
    def (ys, zs): split(xs)
    merge(mergesort(ys), mergesort(zs))</code></pre></div>

<div class="code-block"><pre><code class="language-python">def movingAvg(ys, window):
  [sum([nthPad(n, ys)
        for n in range(i - window, i + window)])
     / (1 + 2 * window)
   for i in range(0, length(ys) - 1)]</code></pre></div>

	</div>
	<div></div>
</Grid>

<style>
	.code-block {
		border-radius: 8px;
		margin: 1em 0;
		overflow-x: auto;
	}

	.code-block pre {
		margin: 0;
		padding: 1em 1.2em;
		border-radius: 8px;
		font-size: 13px;
		line-height: 1.5;
	}
</style>
