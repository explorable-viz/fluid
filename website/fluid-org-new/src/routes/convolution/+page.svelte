<script lang="ts">
   import { CodeMirror, DataPane, Figure, Grid } from '@explorable-viz/fluid';
   import SubHeader from '$lib/SubHeader.svelte'

	let showDataPane = $state(false);

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['inputImage', 'filter'],
		query: false,
		linking: false
	};

	const convolution = '../fluid/convolution.fld';
	const matrix = '../fluid/lib/matrix.fld';
</script>

<Grid {showDataPane}>
	<SubHeader />

	<div class="flex-left-align">
		<p>
			<span class="highlight">Explorable explanations</span> are interactive web essays that explain
			challenging technical ideas. This elegant
			<a href="https://distill.pub/2019/computing-receptive-fields/">distill.pub article</a>
			explains <span class="highlight">matrix convolution</span> and related ideas like
			<span class="highlight">receptive field</span>, important notions in CNNs that also have
			applications in image processing. Educational efforts like these are valuable but
			labour-intensive, especially for the kind of interactive graphics we might want to show how an
			algorithm like convolution works.
		</p>

		<p>
			How could a language like Fluid help? For an interactive explanation of an algorithm, one
			possibility is to use Fluid's built-in provenance-tracking infrastructure to allow a user to
			explore the relationships between the stages of the convolution pipeline, using interactions
			like the ones shown below. This moves a real implementation closer to being a <span
				class="highlight">self-explanatory artifact</span
			>, reducing the need for separate, custom-crafted explanations. Enriched with integrated
			documentation, “open implementations” like these could form the basis of a kind of
			<span class="highlight">literate execution</span> and way of authoring explorable explanations with
			less effort.
		</p>
	</div>

	<DataPane {showDataPane} toggle={() => (showDataPane = !showDataPane)}>
		<div id="fig-data-pane" class="flex-right-align data-pane">
			<div id="fig-input" class="data-pane-column"></div>
			<div id="fig-intermediate" class="data-pane-column"></div>
		</div>
	</DataPane>
	<div class="flex-left-align">
		<div class="flex-bottom-align">
			<Figure {spec} fld={convolution} />

			<div>
			</div>
		</div>
	</div>
	<div></div>
	<div></div>
	<div class="flex-left-align">
		<CodeMirror name="matrix" src={matrix} />
		<p class="right-justified"><a href="../convolution-wrapped/">continued..</a></p>
	</div>
</Grid>

<style>
	.when-data-pane-visible {
		font-size: 10pt;
	}
</style>
