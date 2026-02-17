<script lang="ts">
	import DataPane from '$lib/components/DataPane.svelte';
	import Figure from '$lib/components/Figure.svelte';
	import Grid from '$lib/components/Grid.svelte';
	import SubHeader from '$lib/components/SubHeader.svelte';

	let showDataPane = $state(false);

	const spec = {
		fluidSrcPath: ['../fluid'],
		inputs: ['inputImage', 'filter'],
		query: false,
		linking: true
	};

	const convolution = '../fluid/convolution-wrapped.fld';
</script>

<Grid {showDataPane}>
	<SubHeader />
	<div class="flex-left-align">
		<p>
			Contrast the <a href="/convolution">previous</a> example, which used
			<code>zero</code>
			as the boundary method, with this one, which uses <code>wrap</code>. Toggle the
			<span class="highlight">data pane</span>
			again; then select an output cell near the edge. This implementation has a quite different behaviour:
			instead of some of the <code>filter</code> elements becoming irrelevant, we can see visually
			that the <code>inputImage</code> behaves as though opposite sides were connected. These examples
			show how just making the input-output relationships fine-grained rather than monolithic already
			reveals quite a lot about what a computation actually does.
		</p>
	</div>
	<DataPane {showDataPane} toggle={() => (showDataPane = !showDataPane)}>
		<div id="fig-data-pane" class="flex-right-align data-pane">
			<div id="fig-input" class="data-pane-column"></div>
			<div id="fig-intermediate" class="data-pane-column"></div>
		</div>
	</DataPane>
	<div class="flex-left-align">
		<Figure {spec} fld={convolution} />
		<p>What happens now at the corners of the output matrix?</p>
		<p class="right-justified"><a href="/convolution">previous..</a></p>
	</div>
</Grid>
