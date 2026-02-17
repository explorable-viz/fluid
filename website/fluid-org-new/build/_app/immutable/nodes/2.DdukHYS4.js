import{f as s,a as o}from"../chunks/D2M_aK7b.js";import{p as y,a as v,f as b,s as r,b as k,g as n,d as x,c as T,n as D,r as _}from"../chunks/C5WKpRwH.js";import{C as N}from"../chunks/Ds6RtFwm.js";import{b as P,S as R,D as C}from"../chunks/BiMlg1xD.js";import{o as F}from"../chunks/CwsymhHL.js";import{G as O}from"../chunks/CcJ7_MrR.js";const S=`import nonRenewables

def countries: ["BRA", "EGY", "IND", "JPN"]

def totalFor(year, country):
  def [row]:
    [row for row in nonRenewables if row.year == year if row.country == country]

  (row.nuclearOut + row.gasOut + row.coalOut) + row.petrolOut

def stack(year):
  [{ y: country, z: totalFor(year, country) } for country in countries]

def yearData(year):
  [row for row in nonRenewables if row.year == year if row.country |elem| countries]

MultiView([
  BarChart({
    caption: "Non-renewables output",
    size: { width: 275, height: 185 },
    tickLabels: { x: Default, y: Default },
    stackedBars:
      [{ x: num_to_str(year), segments: stack(year) } for year in [2014 .. 2018]],
    legend: True
  }),
  ScatterPlot({
    caption: "",
    points:
      [{
        x: sum([row.nuclearOut for row in yearData(year)]),
        y: sum([row.nuclearCap for row in yearData(year)])
      } for year in [2014 .. 2018]],
    labels: { x: "Nuclear capacity", y: "Nuclear output" }
  })
])
`;var M=s('<div id="fig"><div class="fig-loading svelte-wilu6i">loading figure(s)</div></div>');function Y(t,e){y(e,!0),F(()=>{P(e.spec)("fig")(e.src)()});var a=M();o(t,a),v()}var A=s(`<div class="flex-right-align data-pane svelte-1uha8ag"><div class="data-pane-column"><div id="fig-input"><p>Tracking how data flows through a computation allows us to do more than just hide away
						or grey out unused data. Try some of the following:</p> <ul><li>Mouse over the <code>nuclearCap</code> data. Various points in the scatter plot will be
							highlighted in blue as you move around: these are the outputs which consume the data element
							under your mouse.</li> <li>As you move over <code>nuclearCap</code> values, notice that three other values are
							also highlighted with the blue border. These are its <span class="highlight">related inputs</span>: the other inputs needed to compute the
							scatter plot point. Each point in the scatter plot represents a year; for a given
							year, the nuclear capacities of 4 countries were added together to compute the
							x-coordinate, which is why in this case we see 4 mutually related values.</li> <li>Now try the same with <code>nuclearOut</code>. Now the related inputs also include the
							coal, gas and petrol output for that country. That's because these data are also used
							to compute the <span class="highlight">bar segments</span> in the bar chart. You'll see the various bar
							segments being highlighted with a thin border as you move around in that column.</li></ul></div></div></div>`),B=s(`<!> <div class="flex-left-align"><p>With traditional print media, the figures, text and other content are disconnected from the
			underlying data, making them hard to understand, evaluate and trust. Digital media, such as
			online papers and articles, present an opportunity to make visual artifacts which are
			connected to data and able to reveal those fine-grained relationships to an interested user.
			This would enable research outputs, news articles and other data-driven artifacts to be more
			transparent, self-explanatory and explorable &mdash; a nice goal but one which would be
			impractical if we had to implement these features by hand for each output. <span class="highlight">Fluid</span> is a programming language that tracks how data flows through a computation and makes it possible
			to author computational outputs where various transparency features are built-in.</p> <p>When the figures below have finished loading, click on the button in the margin to reveal the <span class="highlight">data pane</span>. Records are shown only if any of their fields are
			used somewhere in either of the two figures. Records that are completely unused &mdash; as
			well as any unused fields of other records, which are greyed out &mdash; are called <span class="highlight">inert</span>. By hiding inert data, we can present the reader with a
			view containing only the used data (a useful default setting).</p></div> <!> <div class="flex-left-align"><!> <p>You can also interact with the output. Try the following:</p> <ul><li>Move your mouse over segments in the bar chart. You'll see the data needed to compute the
				height of the segment. You'll also see that there is always a scatter plot point highlighted
				on the right. This secondary highlighting picks out the <span class="highlight">related outputs</span>: the other outputs that make use of the selected data. This feature is called <span class="highlight">brushing and linking</span> in data visualisation, but here we can do it in a transparent way. Related input and related outputs
				are <span class="highlight">dual</span> notions.</li> <li>Now click on one of the bar segments. That turns the highlighting on the bar into a <span class="highlight">persistent selection</span>; the bar will become darker and the corresponding input selection will turn green. Now
				move your mouse over to the scatter plot. With the first selection still active, you can now
				interact with different scatter plot points to see how the data they use intersects with the
				data for the bar segment. In particular any scatter plot points which are “related” to the
				selected bar segment will demand data which overlaps with the data needed for the bar
				segment.</li></ul> <h4>Computational transparency as infrastructure</h4> <p>This only scratches the surface of what is possible, but hints at how we might help a reader
			understand or evaluate a research paper or news article in situ. The key idea is that the
			transparency features are provided automatically; the author of the content need only express
			their visualisation as a pure function of the input. As the infrastructure improves, the
			transparency benefits become available to all users, with no additional effort required on the
			part of authors. Here is the Fluid code for the figures above:</p> <!> <p>There are several limitations of the current system, as well as lots of directions in which we
			plan to move things forward; see the <a href="/faq">FAQ</a> for details.</p></div>`,1);function j(t){let e=x(!1);const a={fluidSrcPath:["fluid"],inputs:["nonRenewables"],query:!1,linking:!0},c="/fluid/non-renewables.fld",p=S;O(t,{get showDataPane(){return n(e)},children:(g,q)=>{var i=B(),l=b(i);R(l,{});var h=r(l,4);C(h,{get showDataPane(){return n(e)},toggle:()=>k(e,!n(e)),children:(w,G)=>{var m=A();o(w,m)}});var u=r(h,2),d=T(u);Y(d,{get spec(){return a},get src(){return p}});var f=r(d,10);N(f,{name:"non-renewables",src:c}),D(2),_(u),o(g,i)}})}export{j as component};
