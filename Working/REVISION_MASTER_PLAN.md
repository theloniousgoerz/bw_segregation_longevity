# Strategic Revision Master Plan

### "Racial Segregation and Black–White Longevity Disparities" — AJS MS 412494

**Target resubmission:** new submission to AJS **Source reviews:** Editor (John Levi Martin), Reviewer A, Reviewer E, Reviewer I

------------------------------------------------------------------------

## Orientation: what the rejection actually says

The AJS decision is a **reject**, and the editor is explicit on two points that frame everything below:

1.  An "improved" version of *this* paper will **not** be re-reviewed at AJS. The board would only welcome a *different* paper from the same project — one that drops the current "triad of argument–data–method."
2.  All three reviewers and the editor converge on the **same core defect**: the paper "gestures to a causal argument it cannot nail down." This is not a polish problem. The critical path runs through **identification + theoretical mechanism**, not through tables and typos.

~~Two upstream **decisions** dominate the entire graph and must be made first (see Phase 5.3): **Framing fork** — descriptive vs. explanatory paper (Reviewer A). **Outcome definition** — longevity *level* vs. Black–White *gap* (Reviewer I).~~ **Both decisions are now made and executed in the manuscript** (explanatory; levels). See Status Update 2026-07-28.

------------------------------------------------------------------------

## Status Update — 2026-07-28 (draft audit)

Audited against `Working/Goerz_Segregation_Draft.tex` (653 lines) as of this date. **The revision is no longer a plan — it is a substantially rewritten manuscript.** Both bottleneck decisions are made, the identification strategy has been replaced wholesale, and the theoretical frame has been rebuilt around a three-process typology plus a political-economy mechanism.

**What the draft now is:**

- New title: *Segregation, Political Economy, and The Longevity of Black and White Americans*.
- New framing: Figure 1 formalizes three processes (mutual harm / zero-sum / Black-specific harm) all consistent with an observed Black–White gap; the paper estimates **race-specific levels** and recovers the gap as a derived quantity.
- **The original Cutler–Glaeser government-fragmentation instrument is gone.** Replaced by (1) Ananat's pre-1911 Railroad Division Index, (2) TIGER natural rivers/streams density, (3) sibling fixed effects (exact + flexible matching).
- New mechanism section + results: county tax revenue, health/hospital, Medicaid, and welfare expenditure from the Government Finance Database; stepwise attenuation shows 33.5% (Black) / 12.7% (White) of the effect absorbed.
- New data added: Atack railroad shapefiles, TIGER hydrography, GFD county finance, sibling links, CenSoc Army enlistment records, county PM2.5.
- N grows 2,094,017 → 2,412,213.
- Headline estimates changed: Black −0.44 → **−0.62**; White −0.29 → **−0.53**.

**Author decisions recorded:** multilevel models (RA.2) and MSEM (RA.2c) are **declined** and answered in the text via a cluster-robust-SE justification (draft ll. 354–365). Discussion and Conclusion are **still in outline form and excluded from this audit** per author instruction.

------------------------------------------------------------------------

## Phase 1 — Atomic Parsing (marked up)

| SourceID | Quote (abbreviated) | Atomic Task |
|----|----|----|
| EiC.1 | "gesturing to a causal argument that it cannot really nail down… no opportunity to specify a clear mechanism" | ~~Strengthen the causal logic / specify and test a mechanism *(= RE.3, RI.3)*~~ **DONE** — §Political Economy of Places motivates the mechanism; §Political-Economic Mechanisms of Segregation tests it (Table 2, Fig. 8). Causal logic now written out in §Empirical Strategy. |
| EiC.2 | "is the exclusion restriction really satisfied? If so, why?" | ~~Defend the instrument's exclusion restriction *(= RI.6)*~~ **DONE** — the attacked instrument is dropped entirely. Draft ll. 304–312 give a direct defense: RDI→education/income tests (A6/A7), RDI→PM2.5 test (A8), South-restriction problem and its fix, natural-boundary instrument as a design that needs no human-placement assumption, sibling FE as a design that needs no exclusion restriction at all. |
| EiC.3 | "the strong selectivity involved in residential location" | ~~Address selection into where people live~~ **MOSTLY** — IV addresses county-level sorting, sibling FE absorbs family-level selection, birth-county FE absorbs origin. ⚠️ *Remaining:* health-selective moves in late life (see RE.6, still open) are not directly probed. |
| EiC.4 | "difficulties of pinning down age-specific processes that would link exposure to outcomes" | ~~Address age-specific exposure→outcome processes *(= RE.2)*~~ **DONE** — footnote at draft l. 178 states the age-specific framing explicitly; each neighborhood mechanism now carries an older-adult passage (weathering, culture of fear, housing quality → disability/CRP). |
| EiC.5 | "the problematic nature of some of the matched census data for examining racial differences over these cohorts" | ~~Assess matched-census data quality for racial comparisons~~ **DONE** — draft l. 236: 22% match rate, race-specific −1.8/−2.4 pp under-representation, ABE selectivity for unmarried women, post-stratification weights re-run in Appendix A2. Table 1 now reports representativeness **by race across all three estimation samples**. |
| EiC.6 | "try different identification strategies and different instruments" | ~~Test alternative instruments / identification~~ **DONE** — RDI, rivers, exact + flexible sibling FE, all reported and assembled in Fig. 7. |
| EiC.7 | "assessed the weight of the evidence for explaining any observed association" | ~~Provide a weight-of-evidence / sensitivity assessment~~ **DONE** — four-strategy triangulation figure + appendix battery (weights, alt. segregation measures, region, cohort, military service, gender, education, monotonicity, Gompertz correction). |
| RA.1 | "falls between two stools… needs to move more decisively to one of these approaches" | ~~DECIDE descriptive vs. explanatory framing~~ **DECIDED: explanatory/causal**, and stated as such throughout. |
| RA.1b | "front-end of the paper should connect in a straightforward fashion to the constructs discussed in the back-end" | ~~Tighten front-end to map onto back-end~~ **DONE** — front-end now sets up exactly two back-end objects: the three-process typology (→ race-stratified estimates) and political economy (→ mechanism decomposition). |
| RA.1c | "the key difference in Logan and Parman (2018) and Karbeah and Hacker (2023) is *not* that their historical measures… suffer from data limitations" | **MOSTLY** — the mischaracterization is deleted (draft l. 174 now cites single-state coverage and correlational design). ⚠️ *Remaining:* the positive point the reviewer asked for (granularity / MAUP / subpopulation differences) is not made. One or two sentences. |
| RA.1d | "mentioning these 'dynamic environments' (pp.10-11) simply subjects your conceptualization… to additional scrutiny" | ~~Prune activity-space / dynamic-environment material~~ **DONE** — cut entirely (Browning, Cagney, Sharp, Anderson, Hicken all removed). |
| RA.1e | "emphasize mechanisms for which you have empirical proxies… use the Montez et al. dataset… 18 policy domains" | ~~Add Montez et al. policy controls; emphasize proxied mechanisms~~ **DONE, via substitution** — county Government Finance Database measures used instead of the state-level Montez index, justified at draft l. 206 as an analogous county-level construct. Montez cited. ⚠️ Be ready to defend the substitution to Reviewer A. |
| RA.2 | "baseline models… (OLS)… not well suited to the multilevel nature of the data… effective sample size… much smaller than 2.1 million" | ~~Replace OLS with multilevel models~~ **DECLINED, ANSWERED IN TEXT** — draft ll. 354–365 argue cluster-robust SEs at county address the same dependence with fewer assumptions on the error process. |
| RA.2b | "expand the set of controls for the county at death to include demographic and industrial composition" | **OPEN** — controls remain individual-level + urban-rural FE + birth-county FE + (in mechanism models) policy channels. No county demographic or industrial composition covariates. ⚠️ **This is now the last unaddressed piece of Reviewer A's empirical critique.** |
| RA.2c | "a series of structural equation models are needed… multilevel structural equation models (MSEM)" | **DECLINED** — but unlike RA.2, the declination is *not yet written*. The mechanism section's post-treatment/intermediate-confounder caveat (draft l. 458) is the right hook; add one sentence saying why stepwise attenuation-with-bounds is preferred over MSEM here. |
| RA.2d | "'it is reasonable to believe that place characteristics are highly correlated with experience at birth' (p.18)… a leap of faith" | ~~Resolve birth-county FE interpretation~~ **DONE** — the leap-of-faith sentence is gone; draft ll. 245–246 now concede the intervening-exposure gap openly and label it a limitation. |
| RA.3 | "the models are too spare for us to have much confidence in these estimates" | **PARTIAL** — richer *identification* (4 strategies), not richer *covariates*. Closing RA.2b closes this. |
| RA.4 | "several typos… 'full fount'… repetition… awkward text in footnote 5" | **OPEN — and larger than before.** ~60+ misspellings in the current draft (sheilding, sepending, plusibly, inequalites, ambigous, advangage, raical, disenfranchize, pathwways, segreation, demogarphy, accress, machanisms, environental, Beacuse, engogenous, traingulate, vriety, asusmptions, validiety, startified, Instrumantal, analyeses, taxe, poliicy, coeffcient, peices, specfic, udnerstanding, storng, asessement, individuls, gaurantee, occured, insfrastructure, motality, minumum, Futhermore, conext, decrese …). Also `monotonicity_gifure.jpeg` filename. |
| RE.1 | "focusing on the over 65s… may already have selected out… racial crossover…" | ~~Address survivor/selection bias; engage mortality-crossover literature~~ **DONE** — §8.4 *Double Truncation and Mortality Crossover* engages the crossover literature directly (Breen 2026 review; crossover occurs mid-to-late 80s vs. sample modal 67–82) and quantifies truncation bias via the Goldstein et al. Gompertz MLE (+12% Black, +6% White). |
| RE.2 | "more nuance in its treatment of the meaning of segregation among this particular age group… maybe this is not a study of longevity per se" | ~~Add life-course nuance about segregation at 65+~~ **DONE** — outcome explicitly scoped as longevity conditional on survival to 65 and defended; §8.4 does the quantitative work. |
| RE.3 | "would have been considerably stronger if it had tested mechanisms" | ~~Test mechanisms *(= EiC.1)*~~ **DONE.** |
| RE.4 | "move the mechanisms lit review to the discussion section" | ~~Relocate mechanisms lit review to discussion~~ **N/A** — this was the fallback for the descriptive path. Mechanisms are tested, so the lit review belongs up front. |
| RE.5 | "the classic Williams/Collins article… Zinzi Bailey also has a review article" | **HALF** — Williams & Collins (2001) now cited twice (ll. 70, 174) and framed as fundamental-cause. ⚠️ Bailey et al. is in `ref.bib` (`bailey_how_2021`) but **never cited in the text**. One-line fix. |
| RE.6 | "could more be done to examine the role of migration at older ages? … 'returning home to die'… retirement" | **OPEN** — only the `migrated` dummy and the declining-propensity-with-age paragraph (l. 245). No analysis of return/retirement migration. ⚠️ **Given that county-at-death *is* the exposure, this is the most substantively load-bearing open empirical item.** |
| RI.1 | "difficult to differentiate when you were talking about life expectancy… versus a difference in life expectancy… your title suggests… disparities" | ~~Resolve outcome: level vs. gap~~ **DECIDED: levels.** Title changed, typology built on it, gap recovered as a derived difference (l. 433). This is the cleanest fix in the revision. |
| RI.2 | "I needed much more literature along the lines of… section 4 rather than section 3" | ~~Add differential-by-race (gap) literature~~ **DONE** — §Segregation and Black-White Inequalities surveys the White-outcome evidence (Light & Thomas, Quillian, Cutler & Glaeser, Austin, Vu, Logan & Parman, Karbeah & Hacker, Hendi) and organizes it by which of the three processes it implies. |
| RI.3 | "insufficient in motivating why we would expect such a causal relationship… or if… in fact about neighborhood violence, income, crime…" | ~~Motivate a *direct* causal link vs. mediated~~ **DONE** — the paper now explicitly commits to a *mediated* account and contrasts neighborhood-channel vs. political-economy-channel predictions. |
| RI.4 | "you also needed to theoretically motivate why this relationship would be moderated by race" | **PARTIAL — and now partly self-inflicted.** The typology explains *why signs could differ* by race, but nothing explains *why the Black penalty exceeds the White penalty*. Worse: the DuBois / wages-of-Whiteness apparatus that supplied that theory is cut to one sentence (l. 167), while the Discussion outline (l. 494) still promises a "class gradient among Whites → wages-of-Whiteness" payoff, and the education heterogeneity that backed it has been demoted to the appendix. **Front-end and back-end disagree here.** |
| RI.5 | "you never define zero-sum and what hypotheses we would expect… these null hypotheses are straw men" | ~~Define zero-sum; derive hypotheses; drop strawman null~~ **DONE** — footnote at l. 89 distinguishes the game-theoretic definition from the literature's shorthand and declares which is used; Figure 1 derives the three sign patterns; l. 204 states exactly which sign pattern would count as zero-sum. |
| RI.6 | "I wonder about exogeneity… government fragmentation and political context would be correlated with population health" | ~~Defend exclusion restriction~~ **RESOLVED BY REMOVAL** — that instrument no longer appears in the paper. |
| RI.7 | "Section 3.2… became disconnected with this spatial level of measurement" | ~~Reconcile activity-space critique with the county-level measure~~ **RESOLVED BY REMOVAL** — activity-space material cut; the county level is now theorized directly as the *jurisdictional* scale at which taxation and spending are set (l. 188). |
| RI.8 | "I would suggest incorporating more of this [magnitude comparisons] in the literature review and in your findings section" | **OPEN — explicit placeholders in the draft.** l. 435: *"Consider the effects of educational expansion, homeownership, and x. \note{Describe estimate magnitude.}"*; l. 460: *\note{Discuss the estimates in the context of other papers and magnitude of estimates.}* |
| RI.9 | "Page 7 could use some copyedits" | **OPEN** — subsumed into RA.4. |
| RI.10 | "This sentence is surely untrue: 'empirical evidence for the assumption that racial segregation leads to White advantage remains limited.'" | ~~Fix the false claim~~ **DONE** — reworded to *limited empirical **attention*** relative to the Black-outcome literature (ll. 72, 85), and now backed by the Massey (1995) quotation and an explicit survey of what the White-outcome evidence actually shows. |

**Deduplications:** EiC.1 = RE.3 (mechanisms); EiC.2 = RI.6 (exclusion restriction); EiC.4 ≈ RE.2 (age-specific/life-course).

------------------------------------------------------------------------

## Phase 2 — Classification (marked up)

| Tag | Category | Task IDs |
|----|----|----|
| 🔴 STRUCTURAL | Moving/cutting/reorganizing | ~~D_RA2_frontend~~ ✅ · ~~D_RA5_prune_dynamic~~ ✅ · ~~D_RE4_relocate_mechlit~~ ✅ N/A |
| 🟠 ARGUMENTATIVE | Theory / framing / contribution | ~~A_RA1_framing_fork~~ ✅ · ~~A_RI1_outcome_def~~ ✅ · ~~C_RI5_zerosum_def~~ ✅ · **C_RI4_race_moderation `[PARTIAL — front/back mismatch]`** · ~~C_RI3_causal_motivation~~ ✅ · ~~C_RI2_gap_literature~~ ✅ · ~~C_RA3_nonzerosum_case~~ ✅ · ~~C_RA6_proxy_mechanisms~~ ✅ · ~~C_RE2_lifecourse_nuance~~ ✅ · **C_RI8_magnitude `[OPEN — placeholders]`** · **D_capstone_narrative `[OPEN — excluded from audit]`** |
| 🟡 EMPIRICAL | New estimation / data work | ~~A_RA8_multilevel~~ ✅ declined+answered · ~~A_EiC2_RI6_exclusion~~ ✅ · ~~A_EiC6_alt_instruments~~ ✅ · ~~A_EiC3_residential_selection~~ ✅ mostly · ~~A_EiC5_census_quality~~ ✅ · ~~B_RE1_survivor~~ ✅ · ~~B_RE3_mechanisms~~ ✅ · ~~B_RA7_montez_policy~~ ✅ · **B_RA9_county_controls `[OPEN]`** · **B_RA10_msem `[declined, not yet written]`** · **B_RE6_migration `[OPEN]`** · ~~B_EiC7_weight_evidence~~ ✅ |
| 🟢 CLARIFICATION | Definitions / justifications | ~~A_RA11_birthFE_interp~~ ✅ · **C_RA4_logan_karbeah `[mostly — add the positive point]`** · **C_RE5_citations `[half — Bailey uncited]`** · ~~C_RI7_activity_space~~ ✅ · ~~E_RI10_white_advantage_claim~~ ✅ |
| 🔵 EDITORIAL | Typos / copyedits | **E_RA13_typos `[OPEN — large]`** · **E_RI9_p7_copyedits `[OPEN]`** |

**Counts:** 33 tasks → **24 closed · 3 partial · 6 open.**

------------------------------------------------------------------------

## Phase 3–6 — superseded

The dependency graph, critical path, and parallel-batch schedule were built to sequence work that is now largely complete. Blocks A, B (except two items), and C are closed; the remaining work is Block D (Discussion/Conclusion), one empirical gap, and Block E polish. The graph no longer constrains anything. **Superseded by the punch list below.**

Original bottleneck ranking, for the record: ~~A_RA1 framing fork (21 downstream)~~ ✅ · ~~A_RA8 multilevel (16)~~ ✅ declined · ~~A_RI1 outcome def (10)~~ ✅ · ~~B_RE3 mechanisms (8)~~ ✅ · ~~C_RI5 zero-sum def (7)~~ ✅.

------------------------------------------------------------------------

## Remaining punch list — 2026-07-28

### Tier 1 — substantive, referee-facing

1. **B_RE6 — older-age migration.** Not done. Because county-at-death is the treatment, a referee will ask whether retirement/return migration ("going home to die") sorts people into segregated Southern counties in a health-correlated way. You have the birth county, death county, and a migrant flag — a heterogeneity split on migrant status, and ideally a return-to-birth-region indicator, would close this cheaply.
2. **B_RA9 — county-at-death compositional controls.** The only surviving piece of Reviewer A's "too spare" critique. Add county % Black, population/density, and industrial or occupational composition to the fully-adjusted specification. This is more urgent *because* you declined multilevel and MSEM.
3. **RI.4 — race-moderation theory / the DuBois thread.** Decide whether the wages-of-Whiteness argument is in or out. Right now it is out of the front-end and in the Discussion outline, and the education heterogeneity that evidences it sits in the appendix. Either (a) restore a compact DuBois passage to the theory section and promote the education results to the main text, or (b) cut the Discussion promise and let political economy carry the whole explanation. Do not leave it split.
4. **RA.2c — write the one-sentence MSEM declination**, hooked to the post-treatment caveat you already have at l. 458.

> **Not an issue — checked 2026-07-28.** Both IV strategies are *attenuated* relative to OLS, for both groups (per 10 pts of D: OLS −0.71/−0.65; RDI IV −0.62/−0.53; rivers IV −0.33/−0.37). This is the same ordering as the original submission and is already stated correctly at l. 431. No explanation owed.

### Tier 2 — citation and characterization

6. **RE.5** — cite `bailey_how_2021` (already in the bib).
7. **RA.1c** — one or two sentences on what the *actual* difference is with Logan & Parman and Karbeah & Hacker (granularity, MAUP, subpopulation), not just single-state coverage.
8. **RI.8** — fill the two magnitude placeholders (ll. 435, 460). Benchmarks already available from the old draft: Breen homeownership ≈ 0.31 yr; Halpern-Manners education ≈ 0.35 yr/yr. Note the new estimates are *twice* those benchmarks per 10 points of D — anticipate the "too big?" question.

### Tier 3 — drafting holes and mechanics

9. ~~**Incomplete sentence, draft l. 413.**~~ **DONE (2026-07-28)** — sentence completed with the unpaired/paired numbers and an interpretation of the direction.
10. ~~**Placeholder `$X$ counties`, draft l. 275.**~~ **DONE (2026-07-28)** — resolved to $2{,}955$ from `Filtering_table.tex`, which also confirms $N = 2{,}412{,}213$.
11. **`\textbf{[cites]}`** for the PM2.5 → health literature, draft l. 308.
12. **`(Cites)`** for racial differences in welfare receipt, draft l. 452.
13. **Interpret the sibling-FE vs. OLS gap.** `sibling_fe_table.tex` is built for a within-sample unpaired/paired comparison, and adding the family FE moves the estimate *away* from zero: Black −0.61 → −0.94 (exact) and −0.64 → −0.97 (flexible); White −0.65 → −0.79 / −0.80. The draft states the ordering (l. 431) but doesn't interpret it. Sibling FE > OLS is a documented pattern with four candidate explanations:

    - **(a) Shared confounding that biased OLS toward zero** — *the preferred reading.* Family-of-origin advantage plausibly correlates positively with both D and longevity (higher-SES 1940 households → adult destinations in large metro counties, which are both more segregated and better provisioned), i.e. upward bias on a negative coefficient. Differencing it out makes the estimate more negative. Consistent with the paper's existing selection story.
    - **(b) Shared measurement error in D** — the Griliches (1979) result is that within-family differencing amplifies attenuation, *except* when siblings' measurement error is more correlated than their true exposure (r(e₁,e₂) > r(x₁,x₂)). Plausible here: the D index biases already conceded in the Fossett footnote (small-population bias, MAUP, tract-boundary change across 1980/1990/2000) are shared across co-resident siblings. Secondary argument.
    - **(c) Non-shared confounders, amplified** — *the unfavorable reading, and the one to pre-empt.* Frisell et al. (2012) show within-pair estimates are more biased by confounders that differ between siblings than unpaired estimates are. Identification here comes only from siblings discordant in county of death, i.e. from movers — so whatever drives sibling divergence in adult destination is amplified. **This is the same gap as open item 1 (RE.6, older-age migration); closing that partly closes this.**
    - **(d) Effect heterogeneity** — discordant siblings are movers; FE > OLS may reflect a different estimand rather than less bias.

    ⚠️ **Two cautions before drafting.** Ashenfelter & Krueger (1994) is the famous precedent for within-family > OLS but the finding was contested (Bound & Solon 1999; Ashenfelter & Rouse 1998) — cite it for the pattern, not the interpretation. More pressing: **Halpern-Manners et al. (2020), already cited at l. 314 as this paper's sibling-FE precedent, found sibling/twin estimates modestly *attenuated* relative to baseline** — same era, same 1940-census-plus-mortality design, opposite direction. Address it rather than let a referee raise it.

    **Cites to add to `ref.bib`** (none currently present): Frisell, Öberg, Kuja-Halkola & Sjölander 2012, *Epidemiology* 23(5):713–20; Griliches 1979, *JPE* 87(5):S37–64; Ashenfelter & Krueger 1994, *AER* 84(5); Bound & Solon 1999, *Economics of Education Review* 18(2).
14. ~~**Figure cross-references off by one.**~~ **DONE (2026-07-28)** — every hard-coded figure number in the body converted to `\ref{}`, a `\label` added to the Gompertz figure, and the "Figure X" placeholder resolved. Draft compiles with no unresolved references.
15. **`\note{}` at l. 310** flags a live bug: `Analysis/06_Robustness_Checks.R:120` fits three pairwise instrument-correlation models including `ln_gov`, a variable from the *old* government-fragmentation design that no longer appears in the manuscript. Re-running that script as written adds a column for an undefined instrument. Fix the script and the passage.
16. **Rename `monotonicity_gifure.jpeg`.**
17. **E_RA13 / E_RI9 — full copyedit pass.** ~60+ misspellings; the abstract alone has "sepending," "sheilding" is in the first paragraph of the intro. Do this last but do not skip it — a reject-and-resubmit that arrives with typos in the abstract reads as careless.
18. **Strip the `\note{}` and `\heldout{}` scaffolding** before submission (the preamble already provides the kill-switch redefinitions at ll. 43, 49).

### Excluded from this audit

**Discussion and Conclusion** (draft ll. 483–506) are still an enumerated outline. Per author instruction, not assessed.
