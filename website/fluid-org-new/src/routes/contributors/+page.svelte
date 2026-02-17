<script lang="ts">
	import { contributors, funders } from '$lib/config/contributors';
</script>

<div id="grid" class="grid-container data-pane-visible">
	<div class="flex-right-align data-pane">
		{#each funders as { url, logo, name }, index (index)}
			<a href={url}><img src={logo} alt="{name} logo" width="250px" /></a>
		{/each}
	</div>
	<div></div>
	<div class="flex-left-align">
		<h3 class="title">Funders</h3>
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
			Since September 2022 the project has been based in the <a
				href="https://plrg-bristol.github.io/">Programming Languages Research Group</a
			>, School of Computer Science, University of Bristol, and since September 2023 has been a
			collaboration with the
			<a href="https://iccs.cam.ac.uk/">Institute of Computing for Climate Science</a>, University
			of Cambridge. ICCS is funded by a grant from Schmidt Sciences.
		</p>

		<h3 class="title">Contributors</h3>
		<p>The following people have contributed to this project:</p>
		<table>
			<tbody>
				{#each contributors as { name, url, organisation }, index (index)}
					<tr class="table-row">
						<td class="table-cell"><a href={url}>{name}</a></td>
						<td class="table-cell">{organisation}</td>
					</tr>
				{/each}
			</tbody>
		</table>
	</div>
</div>

<style>
	table td:first-child {
		padding-right: 1em;
	}

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
