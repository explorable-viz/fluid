<script lang="ts">
	import { supporters } from '$lib/config/supporters';
   import { Grid } from '@explorable-viz/fluid';
</script>

<Grid showDataPane="false">
	<div class="flex-right-align data-pane">
		{#each supporters as { url, logo, name }, index (index)}
			<a href={url}><img src={logo} alt="{name} logo" width="250px" /></a>
		{/each}
	</div>
	<div></div>
	<div class="flex-left-align">
		<h3 class="title">Supporters</h3>
		<p>
			An initial 5-month <a
				href="https://www.turing.ac.uk/research/research-projects/data-science-toolkit-explorable-data-visualisations"
				>pilot project</a
			>
			(January through May 2019) was supported by The Alan Turing Institute, through the
			<a href="https://www.turing.ac.uk/research/research-programmes/tools-practices-and-systems"
				>Tools, Practices and Systems</a
			>
			programme. See <a href="https://f.luid.org/0.3.1/">v0.3.1</a> online demo.
		</p>

		<p>
			A follow-up 6-month project (November 2021 to April 2022) was supported by <a
				href="https://www.turing.ac.uk/about-us/impact/asg">AI for Science and Government</a
			>
			programme of The Alan Turing Institute. See
			<a href="https://f.luid.org/0.6.1/">v0.6.1</a> online demo.
		</p>

		<p>
			Since September 2022 the project has been a collaboration between the <a
				href="https://plrg-bristol.github.io/">Programming Languages Research Group</a
			>, School of Computer Science, University of Bristol, the
			<a href="https://iccs.cam.ac.uk/">Institute of Computing for Climate Science</a>, University
			of Cambridge, and the
			<a href="https://www.kent.ac.uk/school-of-computing">School of Computing</a>, University of
			Kent. ICCS is funded by a grant from Schmidt Sciences.
		</p>
	</div>
</Grid>

<style>
	.data-pane {
		margin-top: 10pt;
		padding-right: 5pt;
	}

	.data-pane > :not(:last-child) {
		border-bottom: 0.5px solid lightgray;
		padding-bottom: 2px;
	}

	.data-pane > :not(:first-child) {
		padding-top: 2px;
	}
</style>
