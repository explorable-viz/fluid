<script>
	import NonRenewables from '$lib/assets/fld/non-renewables.fld?raw';
   import { CodeMirror, DataPane, FigureSrc, Grid } from '@explorable-viz/fluid';

	let showDataPane = $state(false);

	const spec = {
		fluidSrcPath: ['fluid'],
		inputs: ['nonRenewables'],
		query: false,
		linking: true
	};

	const fld = '/fluid/non-renewables.fld';
	const src = NonRenewables;
</script>

<Grid {showDataPane}>
	<div class="flex-left-align">
		<h3 class="title">Transparent research outputs</h3>
		<p>
			With traditional print media, the figures, text and other content are disconnected from the
			underlying data, making them hard to understand, evaluate and trust. Digital media, such as
			online papers and articles, present an opportunity to make visual artifacts which are
			connected to data and able to reveal those fine-grained relationships to an interested user.
			This would enable research outputs, news articles and other data-driven artifacts to be more
			transparent, self-explanatory and explorable &mdash; a nice goal but one which would be
			impractical if we had to implement these features by hand for each output. <span
				class="highlight">Fluid</span
			> is a programming language that tracks how data flows through a computation and makes it possible
			to author computational outputs where various transparency features are built-in.
		</p>
	</div>

	<DataPane {showDataPane} toggle={() => (showDataPane = !showDataPane)}>
		<div class="flex-right-align data-pane">
			<div class="data-pane-column">
				<div id="fig-input">
				</div>
			</div>
		</div>
	</DataPane>

	<div class="flex-left-align">
		<FigureSrc {spec} {src} />
		<CodeMirror name="non-renewables" src={fld} />
	</div>
</Grid>

<style>
	.data-pane {
		max-width: 475px;
	}
</style>
