# Strategic Revision Master Plan

### "Racial Segregation and Black–White Longevity Disparities" — AJS MS 412494

**Target resubmission:** Social Forces *or* Demography **Source reviews:** Editor (John Levi Martin), Reviewer A, Reviewer E, Reviewer I

------------------------------------------------------------------------

## Orientation: what the rejection actually says

The AJS decision is a **reject**, and the editor is explicit on two points that frame everything below:

1.  An "improved" version of *this* paper will **not** be re-reviewed at AJS. The board would only welcome a *different* paper from the same project — one that drops the current "triad of argument–data–method." This rules AJS out as a near-term target and makes **Social Forces / Demography the right venues**.
2.  All three reviewers and the editor converge on the **same core defect**: the paper "gestures to a causal argument it cannot nail down." This is not a polish problem. The critical path runs through **identification + theoretical mechanism**, not through tables and typos.

Two upstream **decisions** dominate the entire graph and must be made first (see Phase 5.3): - **Framing fork** — descriptive vs. explanatory paper (Reviewer A). - **Outcome definition** — longevity *level* vs. Black–White *gap* (Reviewer I).

Everything downstream is conditional on these.

------------------------------------------------------------------------

## Status Update — 2026-06-30

**Venue decision:** resubmitting as a *new* submission to AJS (per the editor's offer), not to Social Forces/Demography. This keeps the bar described above in play — the new submission must read as a genuinely different argument–data–method triad, not a polished version of the old one.

**Verification round completed:** new instruments, sibling-FE robustness, and mechanism/policy-channel models have been estimated and checked. **No manuscript writing has started yet.** Items below tagged `[COMPLETED]` reflect finished *analysis*, not finished *prose*. Any task that means "motivate," "relocate," "define," or "frame" in writing is still fully `[OPEN]` regardless of the analysis behind it.

**Block A GO/NO-GO gate: PASSED.** Three instruments (railroad/RDI, rivers, government fragmentation) plus exact and flexible sibling-FE models converge rather than diverge — the causal/explanatory path remains viable; Phase 5.2's pivot-to-descriptive trigger is not activated.

------------------------------------------------------------------------

## Phase 1 — Atomic Parsing

| SourceID | Quote (abbreviated) | Atomic Task |
|----|----|----|
| EiC.1 | "gesturing to a causal argument that it cannot really nail down… no opportunity to specify a clear mechanism" | Strengthen the causal logic / specify and test a mechanism *(= RE.3, RI.3)* `[PARTIAL — mechanism tested empirically (policy-mediation channel); causal-logic narrative not yet written]` |
| EiC.2 | "is the exclusion restriction really satisfied? If so, why?" | Defend the instrument's exclusion restriction *(= RI.6)* |
| EiC.3 | "the strong selectivity involved in residential location" | Address selection into where people live |
| EiC.4 | "difficulties of pinning down age-specific processes that would link exposure to outcomes" | Address age-specific exposure→outcome processes *(= RE.2)* |
| EiC.5 | "the problematic nature of some of the matched census data for examining racial differences over these cohorts" | Assess matched-census data quality for racial comparisons |
| EiC.6 | "try different identification strategies and different instruments" | Test alternative instruments / identification `[COMPLETED — RDI/railroad, rivers, gov.-fragmentation IVs + exact/flexible sibling FE, all convergent]` |
| EiC.7 | "assessed the weight of the evidence for explaining any observed association" | Provide a weight-of-evidence / sensitivity assessment |
| RA.1 | "falls between two stools… needs to move more decisively to one of these approaches" | DECIDE descriptive vs. explanatory framing |
| RA.1b | "front-end of the paper should connect in a straightforward fashion to the constructs discussed in the back-end" | Tighten front-end to map onto back-end |
| RA.1c | "the key difference in Logan and Parman (2018) and Karbeah and Hacker (2023) is *not* that their historical measures… suffer from data limitations" | Correct mischaracterization of Logan & Parman / Karbeah & Hacker (granularity, MAUP, subpopulation) |
| RA.1d | "mentioning these 'dynamic environments' (pp.10-11) simply subjects your conceptualization… to additional scrutiny" | Prune activity-space / dynamic-environment material |
| RA.1e | "emphasize mechanisms for which you have empirical proxies… use the Montez et al. dataset… 18 policy domains" | Add Montez et al. policy controls; emphasize proxied mechanisms |
| RA.2 | "baseline models… (OLS)… not well suited to the multilevel nature of the data… effective sample size… much smaller than 2.1 million" | Replace OLS with multilevel models |
| RA.2b | "expand the set of controls for the county at death to include demographic and industrial composition" | Expand county-at-death controls |
| RA.2c | "a series of structural equation models are needed… multilevel structural equation models (MSEM)" | Estimate MSEM (explanatory path) |
| RA.2d | "'it is reasonable to believe that place characteristics are highly correlated with experience at birth' (p.18)… a leap of faith" | Resolve birth-county FE interpretation (recent vs. historical) |
| RA.3 | "the models are too spare for us to have much confidence in these estimates" | Re-estimate results with richer models *(executed via RA.2/RA.2b/RA.2c)* |
| RA.4 | "several typos… 'full fount'… repetition… awkward text in footnote 5" | Fix typos/elisions/repetition |
| RE.1 | "focusing on the over 65s… those most subject to the ill effects… may already have selected out… racial crossover… age of death… only half a year apart" | Address survivor/selection bias; engage mortality-crossover literature `[PARTIAL — double-truncation tested via Gompertz model and closely tracks FE estimates; crossover-literature engagement still needed]` |
| RE.2 | "the paper might benefit from more nuance in its treatment of the meaning of segregation among this particular age group… maybe this is not a study of longevity per se" | Add life-course nuance about segregation at 65+ |
| RE.3 | "would have been considerably stronger if it had tested mechanisms… many 'why' questions" | Test mechanisms *(= EiC.1)* `[COMPLETED — policy-channel mediators (taxes, Medicaid, welfare, SNAP, Montez policy-liberalism index) tested as IV mediators]` |
| RE.4 | "move the mechanisms lit review to the discussion section… as opposed to set them up for disappointment" | Relocate mechanisms lit review to discussion |
| RE.5 | "the classic Williams/Collins article… Zinzi Bailey also has a review article" | Add Williams & Collins and Bailey citations |
| RE.6 | "could more be done to examine the role of migration at older ages? … 'returning home to die'… retirement" | Examine older-age migration |
| RI.1 | "difficult to differentiate when you were talking about life expectancy… versus a difference in life expectancy… your title suggests… disparities" | Resolve outcome: level vs. gap |
| RI.2 | "I needed much more literature along the lines of… section 4 rather than section 3" | Add differential-by-race (gap) literature |
| RI.3 | "section 3… insufficient in motivating why we would expect such a causal relationship… or if… in fact about neighborhood violence, income, crime…" | Motivate a *direct* causal link vs. mediated `[PARTIAL — mechanism evidence now supports a mediated (policy) account over a direct-exposure claim; needs to be written up]` |
| RI.4 | "you also needed to theoretically motivate why this relationship would be moderated by race" | Motivate race-moderation theoretically `[PARTIAL — political-economy/policy-mediation channel (segregation → White conservative preferences → tax/welfare policy) gives an explicit race-moderation theory; not yet written into Section 3]` |
| RI.5 | "you never define zero-sum and what hypotheses we would expect… these null hypotheses are straw men" | Define zero-sum; derive hypotheses; drop strawman null |
| RI.6 | "I wonder about exogeneity… government fragmentation and political context would be correlated with population health" | Defend exclusion restriction *(= EiC.2)* |
| RI.7 | "Section 3.2… became disconnected with this spatial level of measurement" | Reconcile activity-space critique with the county-level measure |
| RI.8 | "I would suggest incorporating more of this [magnitude comparisons] in the literature review and in your findings section" | Move magnitude comparisons into lit review/findings |
| RI.9 | "Page 7 could use some copyedits" | Copyedit page 7 |
| RI.10 | "This sentence is surely untrue: 'empirical evidence for the assumption that racial segregation leads to White advantage remains limited.'" | Fix the false "White advantage evidence is limited" claim |

**Deduplications:** EiC.1 = RE.3 (mechanisms); EiC.2 = RI.6 (exclusion restriction); EiC.4 ≈ RE.2 (age-specific/life-course).

------------------------------------------------------------------------

## Phase 2 — Classification

| Tag | Category | Task IDs |
|----|----|----|
| 🔴 STRUCTURAL | Moving/cutting/reorganizing | D_RA2_frontend, D_RA5_prune_dynamic, D_RE4_relocate_mechlit |
| 🟠 ARGUMENTATIVE | Theory / framing / contribution | A_RA1_framing_fork `[decision implied, not yet stated]`, A_RI1_outcome_def `[OPEN]`, C_RI5_zerosum_def `[OPEN]`, C_RI4_race_moderation `[PARTIAL — theory in hand, unwritten]`, C_RI3_causal_motivation `[PARTIAL]`, C_RI2_gap_literature `[OPEN]`, C_RA3_nonzerosum_case `[OPEN]`, C_RA6_proxy_mechanisms `[COMPLETED — analysis]`, C_RE2_lifecourse_nuance `[OPEN]`, C_RI8_magnitude `[PARTIAL — heterogeneity results available as raw material]`, D_capstone_narrative `[OPEN]` |
| 🟡 EMPIRICAL | New estimation / data work | A_RA8_multilevel `[OPEN]`, A_EiC2_RI6_exclusion `[PARTIAL — de-risked by IV triangulation, prose defense not written]`, A_EiC6_alt_instruments `[COMPLETED]`, A_EiC3_residential_selection `[PARTIAL — sibling FE covers family-level selection only]`, A_EiC5_census_quality `[OPEN]`, B_RE1_survivor `[PARTIAL]`, B_RE3_mechanisms `[COMPLETED]`, B_RA7_montez_policy `[COMPLETED]`, B_RA9_county_controls `[OPEN]`, B_RA10_msem `[OPEN]`, B_RE6_migration `[OPEN]`, B_EiC7_weight_evidence `[PARTIAL — cohort-bin and education-heterogeneity checks now feed this]` |
| 🟢 CLARIFICATION | Definitions / justifications | A_RA11_birthFE_interp, C_RA4_logan_karbeah, C_RE5_citations, C_RI7_activity_space, E_RI10_white_advantage_claim |
| 🔵 EDITORIAL | Typos / copyedits | E_RA13_typos, E_RI9_p7_copyedits |

**Counts:** Structural 3 · Argumentative 11 · Empirical 12 · Clarification 5 · Editorial 2 → **33 tasks**.

The empirical+argumentative mass (23 of 33) confirms this is a deep reconstruction, not a tidy-up.

------------------------------------------------------------------------

## Phase 3 — Dependency Mapping

### Table 1 — Upstream Blockers (selected; full edge set in `revision_tasks.json`)

| Downstream Task | Blocked By | Rationale |
|----|----|----|
| B_RE3_mechanisms | A_RA1, A_RA8 | Mechanism tests require the framing decision and a multilevel estimation framework |
| B_RA10_msem | A_RA1, A_RA8, B_RA7, B_RE3 | MSEM combines proxied mechanisms + policy controls under the explanatory path |
| C_RI3_causal_motivation | A_RA1, B_RE3 | Whether a *direct* causal claim is defensible depends on mechanism results |
| C_RA3_nonzerosum_case | A_RA1, C_RI5 | Cannot make the non-zero-sum case before zero-sum is defined |
| D_RA2_frontend | A_RA1, C_RI5, C_RI3, C_RA3 | Front-end can only be tightened once theory is settled |
| D_capstone_narrative | All D-block + key C-block + B_EiC7 | Final narrative needs settled theory and stable results |
| E\_\* (polish) | D_capstone_narrative | Polish last |

### Table 2 — Collateral Risks

| If you do… | It may affect… | Risk |
|----|----|----|
| A_RA8_multilevel | D_capstone, C_RI8 | Correct effective N may weaken segregation significance; magnitude comparisons shift |
| A_EiC2_RI6_exclusion | D_capstone | If exclusion restriction fails, the causal claim collapses to descriptive |
| A_EiC6_alt_instruments | B_EiC7 | New instruments may give divergent estimates |
| A_EiC5_census_quality | B_RE1 | Race-specific match error could itself explain the near-identical Table 2 ages |
| B_RE3_mechanisms | A_RA1, C_RI3, C_RA6 | No mediating proxy ⇒ explanatory path not viable ⇒ revert to descriptive |
| B_RE1_survivor | D_capstone | Severe survivor selection ⇒ 65+ estimates can't support a "longevity" claim |

### Phase 3b — Structural Validation

> DAG validated: **33 tasks, 52 dependencies, no circular dependencies detected.** Proceed to Phase 4.

------------------------------------------------------------------------

## Phase 4 — Critical Path Sequencing

### Block A — Foundation (decisions + core identification) → **GO/NO-GO gate**

| Priority | Task | Action |
|----|----|----|
| 1 | A_RA1_framing_fork | **Decide** descriptive vs. explanatory `[decision implied by completed work below — explanatory path supported; needs to be formally stated]` |
| 1 | A_RI1_outcome_def | **Decide** level vs. gap outcome `[OPEN]` |
| 2 | A_RA8_multilevel | Move to multilevel estimation (fixes inflated effective N) `[OPEN]` |
| 2 | A_EiC2_RI6_exclusion | Build the exclusion-restriction defense `[PARTIAL — triangulation across 3 IVs is the strongest evidence in hand; prose defense not written]` |
| 3 | A_EiC6_alt_instruments | Try alternative instruments `[COMPLETED]` |
| 3 | A_EiC3_residential_selection | Model residential selection `[PARTIAL — sibling FE addresses family-level selection only]` |
| 3 | A_EiC5_census_quality | Audit matched-census quality by race `[OPEN]` |
| 3 | A_RA11_birthFE_interp | Fix the birth-FE "recent vs. historical" reading `[OPEN]` |

> **GO/NO-GO: PASSED (2026-06-30).** Alternative instruments (RDI/railroad, rivers, government fragmentation) plus sibling-FE models converge rather than diverge — the causal paper remains salvageable. Original trigger ("if the exclusion restriction cannot be defended *and* alternative instruments diverge → pivot to descriptive") is not activated. Note this is evidence *for* the exclusion-restriction defense, not the defense itself — the prose case (A_EiC2_RI6_exclusion) still needs to be written.

### Block B — Sub-analyses & robustness

B_RE1 survivor/crossover `[PARTIAL]` · B_RE3 mechanisms `[COMPLETED]` · B_RA7 Montez policy [expl] `[COMPLETED]` · B_RA9 county controls [desc] `[OPEN]` · B_RA10 MSEM [expl] `[OPEN]` · B_RE6 migration `[OPEN]` · B_EiC7 weight-of-evidence `[PARTIAL]`.

### Block C — Theoretical reframing

C_RI5 define zero-sum `[OPEN]` · C_RI4 race-moderation `[PARTIAL — theory in hand, unwritten]` · C_RI3 direct-causal motivation `[PARTIAL]` · C_RI2 gap literature `[OPEN]` · C_RA3 non-zero-sum case `[OPEN, blocked by C_RI5]` · C_RA4 correct Logan/Karbeah `[OPEN]` · C_RA6 proxied mechanisms `[COMPLETED]` · C_RE5 citations `[OPEN]` · C_RE2 life-course nuance `[OPEN]` · C_RI7 activity-space reconcile `[OPEN]` · C_RI8 magnitude `[PARTIAL]`.

### Block D — Narrative construction

D_RA2 front-end `[OPEN]` · D_RA5 prune dynamic `[OPEN]` · D_RE4 relocate mech-lit `[OPEN — not applicable unless mechanism path is later cut]` · **D_capstone** intro/discussion/conclusion rewrite `[OPEN — no manuscript writing started]`.

### Block E — Polish

E_RA13 typos `[OPEN]` · E_RI9 p.7 copyedits `[OPEN]` · E_RI10 White-advantage claim `[OPEN]`.

```         
BLOCK A ── Foundation: decisions + identification ───────► GO/NO-GO
  A_RA1 framing fork                [CP][BN]                  │
  A_RI1 outcome def                 [BN]                      │
  A_RA8 multilevel                  [BN]                      │
  A_EiC2/RI6 exclusion · A_EiC6 alt-IV · A_EiC3 selection     │
  A_EiC5 census · A_RA11 birth-FE   ← all parallel            │
                                                              ▼
BLOCK B ── Sub-analyses & robustness ────────────────────► new tables
  B_RE3 mechanisms [CP][BN] · B_RE1 survivor · B_RA7/B_RA9    │
  B_RA10 MSEM · B_RE6 migration · B_EiC7 weight-of-evidence   │
                                                              ▼
BLOCK C ── Theoretical reframing ────────────────────────► settled theory
  C_RI5 zero-sum [BN] · C_RI3 causal-motivation [CP]          │
  C_RA3 [BN] · C_RI4 · C_RI2 · C_RA4 · C_RA6 · C_RE5 ·        │
  C_RE2 · C_RI7 · C_RI8        ← largely parallel             │
                                                              ▼
BLOCK D ── Narrative construction ───────────────────────► coherent draft
  D_RA2 front-end [CP] · D_RA5 prune · D_RE4 relocate         │
  D_capstone rewrite [CP][BN]                                 │
                                                              ▼
BLOCK E ── Polish ───────────────────────────────────────► submit
  E_RA13 typos [CP] · E_RI9 p.7 · E_RI10 White-advantage
```

`[CP]` = critical path · `[BN]` = bottleneck (from Phase 6).

------------------------------------------------------------------------

## Phase 5 — Risk & Conflict Resolution

### 5.1 Reviewer conflicts

| Conflict | Position A | Position B | Resolution |
|----|----|----|----|
| Mechanisms | RE/EiC: *test* mechanisms empirically | RE.4 fallback: *relocate* mech-lit to discussion | **Strategic choice tied to the fork.** Explanatory path ⇒ test (B_RE3). Descriptive path ⇒ relocate (D_RE4) and drop the causal claim. Do not do both. `[RESOLVED — B_RE3 executed, mediators confirmed; explanatory path taken. D_RE4 fallback no longer applies unless the mechanism story is later cut in editing.]` |
| Front-end length | RA option A: cut hard | RA option B: keep more, prune selectively | Determined by the fork; net-neutral length either way. |
| Is it even "longevity"? | RE: maybe not, given pre-65 action | Paper's premise: it is | Narrow scope explicitly to *conditional-on-survival-to-65 longevity* and defend, or pivot outcome (A_RI1). |

### 5.2 Process risks

| Risk | Likelihood | Impact | Mitigation |
|----|----|----|----|
| Exclusion restriction indefensible | **High** (3 reviewers flagged) | Fatal to causal claim | Resolve in Block A *before* anything else; have a descriptive fallback ready |
| Multilevel correction kills significance | Medium | High | Run early (A_RA8); if effect dies, descriptive paper still viable |
| No proxy mechanism mediates | Medium | High | B_RE3 gates the explanatory path; revert to descriptive if null |
| Survivor selection too severe | Medium-High | High | B_RE1 quantifies it; may force scope narrowing |
| Montez policy data won't link to county-at-death | Medium | Medium | Confirm linkage feasibility before committing to explanatory path |

### 5.3 Strategic decisions (author input required)

| Decision | Options | Recommendation |
|----|----|----|
| **Framing fork (A_RA1)** | Descriptive · Explanatory | See journal assessment — leans by venue. Descriptive is lower-risk and matches what the data can deliver; explanatory is higher-reward but hinges on B_RE3 + exclusion restriction surviving. `[B_RE3 mechanism tests are now complete and support the explanatory path — recommend formally adopting Explanatory and stating it explicitly in the intro/cover letter. STILL OPEN AS A STATED DECISION.]` |
| **Outcome (A_RI1)** | Longevity level · Black–White gap | Pick **one** and make title, theory, and models agree. The current title promises the *gap*; the models estimate the *level*. `[OPEN — not addressed by this verification round.]` |
| **Causal vs. associational** | Keep IV causal claim · Reframe as well-identified association | If the exclusion restriction cannot be made airtight, reframe as association — this is the editor's explicit advice and de-risks the whole submission. `[OPEN — IV triangulation strengthens the case for "keep causal claim," but the formal write-up of that case has not been done.]` |

------------------------------------------------------------------------

## Phase 6 — Computational Optimization (NetworkX-derived)

### Parallel execution schedule

- **Batch 1 (8):** all Block A tasks — no prerequisites, start immediately.
- **Batch 2 (12):** Block B empirical work + several Block C theory/clarification tasks that depend only on the framing/outcome decisions (C_RI5, C_RI2, C_RI8, C_RA4, C_RE5, C_RI7).
- **Batch 3 (8):** mechanism-dependent theory (C_RI3, C_RA6, C_RE2, C_RI4, C_RA3), MSEM (B_RA10), and two structural moves (D_RA5, D_RE4).
- **Batch 4 (2):** D_RA2 front-end + E_RI10 claim fix.
- **Batch 5 (1):** D_capstone narrative rewrite.
- **Batch 6 (2):** E_RA13 typos + E_RI9 copyedits.

*Optimization note:* Block C theory tasks split across Batches 2–3. The half that depend only on the two decisions (not on mechanism results) can run **in parallel with Block B empirics** — you do not have to wait for results to begin defining zero-sum, fixing the Logan/Karbeah characterization, or adding citations.

### Critical path (minimum timeline = 6 sequential tasks)

1.  **A_RA1** framing fork → `[decision overdue — analysis supports Explanatory; needs to be formally stated]`
2.  **B_RE3** test mechanisms → `[COMPLETED]`
3.  **C_RI3** motivate direct causal link → `[PARTIAL — evidence in hand, not written]`
4.  **D_RA2** restructure front-end → `[OPEN]`
5.  **D_capstone** rewrite narrative → `[OPEN]`
6.  **E_RA13** final polish. `[OPEN]`

Any slip on these six delays the whole revision. Note the path runs *through the explanatory choice*: if you choose the **descriptive** fork, B_RE3 and C_RI3 drop off the critical path and the timeline shortens materially — a concrete reason the descriptive paper is faster to land.

### Bottleneck tasks

| Task | Direct dependents | Total downstream | Note |
|----|----|----|----|
| A_RA1 framing fork | 15 | 21 | Highest-leverage decision in the project `[evidence in hand; decision not yet formally stated]` |
| A_RA8 multilevel | 8 | 16 | Every empirical result depends on getting this right `[OPEN]` |
| A_RI1 outcome def | 4 | 10 | Resolves the level-vs-gap confusion `[OPEN]` |
| B_RE3 mechanisms | 4 | 8 | Gates the explanatory path `[COMPLETED]` |
| C_RI5 zero-sum def | 3 | 7 | Unblocks the whole non-zero-sum argument `[OPEN]` |

Two of the top three bottlenecks are **decisions, not labor** — they cost an afternoon of thinking but block 21 and 10 downstream tasks respectively. Make them first.

### Block validation

A→B→C→D→E ordering holds with no backward edges (no task depends on a later block). The graph is clean.

------------------------------------------------------------------------

## Bottom line

The revision is dominated by **two decisions** (framing fork, outcome definition) and **one make-or-break empirical question** (does the exclusion restriction survive?). Resolve those three before touching prose. The descriptive path is the shorter, safer route; the explanatory path is the higher-ceiling route but lives or dies on B_RE3 and the instrument.

**As of 2026-06-30:** the make-or-break empirical question has been answered favorably — B_RE3 mechanisms are confirmed, alternative instruments converge, and the exclusion restriction is de-risked (though not yet formally defended in prose). The explanatory path is empirically the stronger choice. What remains is almost entirely **writing, not analysis**: formally stating the framing-fork and outcome-definition decisions, writing the causal/mechanism motivation now that the evidence exists, and the full Block D narrative reconstruction. No prose has been drafted yet — treat Block D as the next critical-path item, not the empirical work.
