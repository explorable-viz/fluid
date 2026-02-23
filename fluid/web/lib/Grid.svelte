<script lang="ts">
	let { children, showDataPane } = $props();
</script>

<div id="grid" class="grid-container {showDataPane ? '' : 'data-pane-hidden'}">
	{@render children()}
</div>
