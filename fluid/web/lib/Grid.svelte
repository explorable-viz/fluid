<script lang="ts">
let {
		children,
		showDataPane,
		doubleSize = false
	} = $props();
</script>

<div id="grid" class="grid-container
   {showDataPane ? '' : 'data-pane-hidden'}
   {doubleSize ? 'double-size' : ''}">
	{@render children()}
</div>

<style>
   .grid-container {
      display: grid;
      grid-template-columns: auto var(--toggle-button-width) var(--text-pane-width);
      justify-content: center;
   }

   .grid-container.data-pane-hidden {
      grid-template-columns: 0 var(--toggle-button-width) var(--text-pane-width);
   }

   .grid-container.double-size {
      max-width: 66.7vw; /* undo effect of transform: scale(1.5) for layout purposes */
   }

   .grid-container > :last-child {
      padding-right: 10px;
   }
</style>
