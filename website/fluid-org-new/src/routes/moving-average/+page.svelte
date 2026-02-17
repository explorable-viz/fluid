<script lang="ts">
	import CodeMirror from '$lib/components/CodeMirror.svelte';
	import DataPane from '$lib/components/DataPane.svelte';
	import Figure from '$lib/components/Figure.svelte';
	import Grid from '$lib/components/Grid.svelte';
	import SubHeader from '$lib/components/SubHeader.svelte';

	let showDataPane = $state(false);

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['methane'],
		query: false,
		linking: false
	};

	const movingAverage = '../fluid/moving-average.fld';
</script>

<Grid {showDataPane}>
	<SubHeader />

	<div class="flex-left-align">
		<p>
			As a final example, the following chart shows a moving average (or rolling average) plot for
			methane emissions under projected emissions scenario <a
				href="https://en.wikipedia.org/wiki/Shared_Socioeconomic_Pathways">SSP5-8.5</a
			>. A moving average is essentially a <span class="highlight">1-dimensional convolution</span>
			with a uniform (<a href="https://en.wikipedia.org/wiki/Boxcar_function">boxcar</a>) filter.
			Toggle the <span class="highlight">data pane</span> in the margin, and the try some of the suggestions
			there.
		</p>
	</div>

	<DataPane {showDataPane} toggle={() => (showDataPane = !showDataPane)}>
		<div id="fig-data-pane" class="flex-right-align data-pane">
			<div class="data-pane-column">
				<p>
					How are the <code>year</code> values used? (Try to avoid moving your mouse over
					<code>Agriculture</code> &mdash; these nodes currently participate in a lot of dependency
					edges and so the system is <span class="highlight">currently quite laggy</span> if you do this!)
					You'll see that every year is used in both the original curve and its moving average.
				</p>
				<div id="fig-input"></div>

				<p>
					Now try <code>emissions</code>. The behaviour here is different: each emissions value
					contributes to
					<span class="highlight">one</span> point in the original curve but
					<span class="highlight">three</span>
					in the moving average.
				</p>

				<p>
					As with the other examples, you can also interact with the output. This confirms that each
					point in the moving average consumes 3 emissions inputs and 1 year, as one might expect.
					Interacting with the points in the original curve is a bit laggy at the moment.
				</p>
			</div>
		</div>
	</DataPane>
	<div class="flex-left-align">
		<Figure {spec} fld={movingAverage} />
		<h4>From passive to active reading</h4>
		<p>
			One of our goals is to support comprehension as an <span class="highlight"
				>active inferential activity</span
			>, where the reader not only formulates hypotheses about what they are looking at, but is able
			to actively test those hypotheses via the artifact. In the example above, we can conjecture
			that each moving average point consumes three inputs, representing the “window” for the moving
			average, and test that by interacting with the output. If we could enrich this with additional
			(intensional) information revealing that these three numbers were added together and then
			divided by three, we would be approaching a fully self-explanatory implementation.
		</p>

		<p>
			As before, the infrastructure used to execute and host the visualisations provides these
			interactive transparency features automatically. The moving average itself is computed as a
			pure function of data, as shown below:
		</p>
		<CodeMirror name="moving-average" src={movingAverage} />
	</div>
</Grid>
