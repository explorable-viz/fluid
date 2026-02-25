<script>
	import { DataPane, Figure, Grid } from '@explorable-viz/fluid';

	let showDataPane = false;

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['methane'],
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
	<div class="flex-left-align">
		<h3>Methane</h3>
		<p>Methane emissions example</p>
	</div>

	<DataPane {showDataPane} toggle={toggleDataPane}>
		<div id="fig-data-pane" class="flex-right-align data-pane">
			<div id="fig-input" class="data-pane-column"></div>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<Figure {spec} fld="../fluid/methane.fld" />
	</div>
</Grid>

<style>
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
