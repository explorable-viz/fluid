<script>
	import { DataPane, Figure, Grid } from '@explorable-viz/fluid';

	let showDataPane = false;

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['nonRenewables'],
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
		<h3>Non-Renewable Energy plots</h3>
	</div>

	<DataPane {showDataPane} toggle={toggleDataPane}>
		<div class="flex-right-align data-pane">
			<div id="fig-input"></div>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<Figure {spec} fld="../fluid/non-renewables.fld" />
	</div>
</Grid>

<style>
	:global(.data-pane) {
		max-width: 475px;
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
</style>
