get_otd_results <- function(otd_date, inc_year = "No", as_reactable = "Yes") {

  df <- results_dataset %>%
    dplyr::filter(
      lubridate::month(game_date) == lubridate::month(otd_date),
      lubridate::day(game_date) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(game_date) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      year = lubridate::year(game_date),
      years_ago = lubridate::year(lubridate::now()) - lubridate::year(game_date)
    ) %>%
    dplyr::select(
      years_ago,
      year,
      game_date,
      season,
      game_no,
      opposition,
      venue,
      outcome,
      score,
      goals_for,
      goals_against,
      scorers,
      competition,
      league_tier,
      manager,
      attendance,
      league_pos
    )

  reactable_df <- reactable::reactable(
    data = df,
    meta = list(
      crests = meta_data[['club_crests']]
    ),
    defaultColDef = reactable::colDef(
      headerVAlign = 'bottom'
    ),
    compact = TRUE,
    pagination = FALSE,
    style = list(
      color = "black",
      fontSize = "smaller",
      margin = '5px 0 5px 0'
    ),
    columns = list(
      years_ago = reactable::colDef(
        name = "Year",
        align = "left",
        minWidth = 80,
        cell = reactable::JS("function(cellInfo, state) {
          let years_ago = cellInfo.value;
          let year = cellInfo.row.year;
          let suffix = '';

          if (years_ago === 1) {
            suffix = ' year ago';
          } else {
            suffix = ' years ago';
          }

          return `
            <div style='display:flex; flex-direction:column;'>
              <span style='font-weight:600;'>${year}</span>
              <span style='font-weight:300; color:grey; font-size:0.95em;'>${years_ago}${suffix}</span>
            </div>`
        }"),
        html = TRUE
      ),
      year = reactable::colDef(
        show = FALSE
      ),
      season = reactable::colDef(
        name = "Season",
        align = "left",
        minWidth = 60,
        cell = reactable::JS("function(cellInfo, state) {
          let season = cellInfo.value;
          let game_no = cellInfo.row.game_no;

          return `
            <div style='display:flex; flex-direction:column;'>
              <span style='font-weight:500;'>${season}</span>
              <span style='font-weight:300;color:grey; font-size:0.95em;'>Game ${game_no}</span>
            </div>`
        }"),
        html = TRUE
      ),
      game_date = reactable::colDef(
        name = "Date",
        show = FALSE,
        align = "left",
        minWidth = 90,
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
      ),
      game_no = reactable::colDef(
        name = "Game No.",
        show = FALSE,
        align = "center",
        minWidth = 70
      ),
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 160,
        cell = reactable::JS("function(cellInfo, state) {
          let opponent = cellInfo.value;
          let opponent_text = opponent;
          let venue = cellInfo.row.venue;
          let competition = cellInfo.row.competition;
          let img_src = state.meta.crests[opponent];

          if (venue == 'H') {
            opponent_text = opponent_text.toUpperCase();
          };

          return `
            <div style='display:flex; flex-direction:row;'>
              <div style='display:flex; width:40px; justify-content:space-evenly;'>
                <img src='${img_src}' style='height:32px;' alt='${opponent}'>
              </div>
              <div style='display:flex; flex-direction:column; margin-left: 10px;'>
                <div style='font-weight:500;'>${opponent_text} (${venue})</div>
                <div style='font-weight:300; color:grey; font-size:0.95em;'>${competition}</div>
              </div>
            </div>`;
        }"),
        html = TRUE
      ),
      outcome = reactable::colDef(
        show = FALSE
      ),
      venue = reactable::colDef(
        name = "Venue",
        show = FALSE,
        align = "center",
        minWidth = 70
      ),
      score = reactable::colDef(
        name = "Score",
        show = FALSE,
        align = "center",
        minWidth = 60
      ),
      goals_for = reactable::colDef(
        show = FALSE
      ),
      goals_against = reactable::colDef(
        show = FALSE
      ),
      scorers = reactable::colDef(
        name = "Score",
        minWidth = 165,
        cell = reactable::JS("function(cellInfo, state) {
          let scorers = cellInfo.value;
          let score = cellInfo.row.score;
          score = `<span style='font-weight:600;'>${score}</span>`;

          if (scorers !== undefined) {
            scorers = scorers.split(', ').map(function(scorer) {
              console.log(scorer);
              if (scorer === 'OG') {
                return 'O.G.';
              } else {
                scorer = scorer.split(' ');
                scorer.shift()
                return scorer.join(' ');
              }
            }).join(', ');
            scorers = `<br><span style='font-weight:300;color:gray; font-size:0.95em;'>(${scorers})</span>`;
          } else {
            scorers = '';
          }

          return `${score} ${scorers}`;
        }"),
        html = TRUE
      ),
      competition = reactable::colDef(
        name = "Competition",
        show = FALSE,
        minWidth = 120,
        cell = reactable::JS("function(cellInfo, state) {
          let competition = cellInfo.value;
          let league_tier = cellInfo.row.league_tier;
          let suffix = '';

          const league_tiers = {
            2: 'Second',
            3: 'Third',
            4: 'Fourth',
            5: 'Fifth'
          };

          if (league_tier !== null) {
            suffix = `<span style='color:gray; font-weight:300;'>(${league_tiers[league_tier]} Tier)</span>`;
          }

          return `
            <div style='display:flex; flex-direction:column;'>
              <span>${competition}</span>
              ${suffix}
            </div>`
        }"),
        html = TRUE
      ),
      league_tier = reactable::colDef(
        name = "League Tier",
        show = FALSE,
        align = "center",
        minWidth = 50
      ),
      manager = reactable::colDef(
        name = "Manager",
        minWidth = 75,
        align = "left"
      ),
      league_pos = reactable::colDef(
        name = "Lge Pos.",
        align = "right",
        minWidth = 50,
        cell = reactable::JS("function(cellInfo, state) {
          let pos = cellInfo.value;

          if (pos === undefined) {
            return `<span style='color:gray;'>-</span>`;
          }

          return `${pos}`;
        }"),
        html = TRUE
      ),
      attendance = reactable::colDef(
        name = "Att.",
        minWidth = 60,
        align = "right",
        format = reactable::colFormat(
          separators = TRUE
        )
      )
    )
  )

  if (as_reactable == "Yes") {
    reactable_df
  } else {
    return(df)
  }
}
