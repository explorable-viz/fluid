<script>
	import { DataPane, Figure, Grid } from '@explorable-viz/fluid';

	let showDataPane = false;

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['renewables'],
		query: false,
		linking: true
	};

	const toggleDataPane = () => {
		showDataPane = !showDataPane;
	};
</script>

<Grid {showDataPane}>
	<div></div>
	<div></div>
	<div>
		<h3>Non-renewable energy output</h3>
	</div>

	<DataPane {showDataPane} toggle={toggleDataPane}>
		<div id="fig-data-pane" class="flex-right-align data-pane">
			<div id="fig-input" class="data-pane-column"></div>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<Figure {spec} fld="../fluid/bar-chart-line-chart.fld" />
	</div>
</Grid>

<style>
	:global(.grid-container),
	:global(.grid-container.data-pane-hidden) {
		grid-template-columns: 0.8fr var(--toggle-button-width) var(--text-pane-width);
	}

	:global(.grid-container.double-size) {
		max-width: 66.7vw;
	}

	:global(.data-pane) {
		padding-right: 0.25em;
	}

	:global(.data-pane *) {
		font-size: 10pt;
	}

	:global(.data-pane-column) {
		border-right: 1px dotted #cccccc;
	}
</style>
