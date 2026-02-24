<script>
	import { DataPane, Figure, Grid } from '@explorable-viz/fluid';

	let showDataPane = false;

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['methane'],
		query: false,
		linking: false
	};

	const toggleDataPane = () => {
		showDataPane = !showDataPane;
	};
</script>

<Grid {showDataPane}>
	<div></div>
	<div></div>
	<div class="flex-left-align">
		<h4>Moving Average</h4>
		<p>
			The following chart shows a moving average (or rolling average) plot for methane emissions
			under projected emissions scenario
			<a href="https://en.wikipedia.org/wiki/Shared_Socioeconomic_Pathways">SSP5-8.5</a>.
		</p>
	</div>

	<DataPane {showDataPane} toggle={toggleDataPane}>
		<div class="flex-right-align data-pane">
			<p>
				Every <code>year</code> value is used in both the original curve and its moving average.
			</p>

			<div id="fig-input"></div>

			<p>
				With <code>emissions</code>, each value contributes to
				<span class="highlight">one</span> point in the original curve but
				<span class="highlight">three</span>
				in the moving average.
			</p>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<Figure {spec} fld="../fluid/moving-average.fld" />
	</div>
</Grid>

<style>
	:global(:root) {
		--text-pane-width: 400px;
	}

	:global(.data-pane) {
		max-width: 300px;
	}

	:global(.grid-container),
	:global(.grid-container.data-pane-hidden) {
		grid-template-columns: auto var(--toggle-button-width) var(--text-pane-width);
	}

	:global(.data-pane) {
		padding-right: 0.25em;
	}

	:global(.data-pane *) {
		font-size: 10pt;
	}

	:global(.highlight) {
		font-style: oblique;
	}
</style>
