<script lang="ts">
let {
		children,
		showDataPane,
		doubleSize = false,
      textPaneWidth = doubleSize ? `600px` : '800px'
	} = $props();
</script>

<div
   id="grid"
   class={`grid-container
      ${!showDataPane ? 'data-pane-hidden' : ''}
      ${doubleSize ? 'double-size' : ''}`}
   style={showDataPane
      ? `grid-template-columns: auto var(--toggle-button-width) ${textPaneWidth};`
      : `grid-template-columns: 0 var(--toggle-button-width) ${textPaneWidth};`
   }
>
	{@render children()}
</div>

<style>
   .grid-container {
      display: grid;
      justify-content: center;
   }

   .grid-container.double-size {
      max-width: 66.7vw; /* undo effect of transform: scale(1.5) for layout purposes */
   }

   .grid-container > :last-child {
      padding-right: 10px;
   }
</style>
