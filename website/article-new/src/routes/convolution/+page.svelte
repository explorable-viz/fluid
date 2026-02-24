<script>
	import { CodeMirror, DataPane, Figure, Grid } from '@explorable-viz/fluid';

	let showDataPane = false;

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['inputImage', 'filter'],
		query: true,
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
		<h4>Matrix convolution</h4>
	</div>

	<DataPane {showDataPane} toggle={toggleDataPane}>
		<div id="fig-data-pane" class="data-pane">
			<div id="fig-input" class="flex-right-align data-pane-column"></div>
			<div id="fig-intermediate" class="flex-right-align intermediates"></div>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<div class="flex-top-align">
			<Figure {spec} fld="../fluid/convolution.fld" />
			<div></div>
		</div>
		<CodeMirror name="convolution" src="../fluid/convolution.fld" />
	</div>
</Grid>

<style>
	:global(.grid-container),
	:global(.grid-container.data-pane-hidden) {
		grid-template-columns: 0.8fr var(--toggle-button-width) var(--text-pane-width);
	}

	:global(.data-pane) {
		display: grid;
		grid-template-columns: min-content min-content;
		vertical-align: top;
		justify-content: end;
	}

	:global(.data-pane *) {
		font-size: 9pt;
	}

	:global(.para-text) {
		margin-left: 5pt;
		margin-right: 3pt;
	}

	:global(.intermediates:has(> *)) {
		border-right: 1px dotted #cccccc;
	}

	:global(.matrix-cell.selected-primary-persistent) {
		fill: rgb(214, 240, 214);
	}

	:global(.matrix-cell-text.selected-primary-persistent:not(.matrix-cell-text.selected-primary-transient)) {
		color: green;
	}
</style>
