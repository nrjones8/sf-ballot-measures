// Thank you https://dev.to/ashleemboyer/build-a-multi-page-gatsby-site-from-json-3kp
const path = require('path');
const ballotPropsData = require('../data/ballot_measure_history.json');
const _ = require('lodash');


function prepDataForApexHeatmap(allPropsData) {
    // Format of data for an apex heatmap is a lil confusing, see:
    // https://apexcharts.com/docs/chart-types/heatmap-charts/
    const groupedByDate = _.groupBy(allPropsData, 'election_date');
    let forApex = [];

    // TODO clean this up, there are definitely better ways to do this...
    for (date in groupedByDate) {
        let allProps = groupedByDate[date];
        let propsForThisDate = []

        for (index in allProps) {
          propsForThisDate.push({
            // Required for the heatmap
            x: allProps[index].prop_letter_or_num,
            y: allProps[index].pct_yes_votes - allProps[index].pct_required_to_pass,
          
            // extra context that we care about
            propLetterOrNum: allProps[index].prop_letter_or_num,
            pctYesVotes: allProps[index].pct_yes_votes,
            pctRequiredToPass: allProps[index].pct_required_to_pass,
            numYesVotes: allProps[index].num_yes_votes,
            numNoVotes: allProps[index].num_no_votes,
            description: allProps[index].description,
            title: allProps[index].prop_title, 
          });
        }
        forApex.push({
            // These two are necessary for apex's heatmap
            name: date,
            data: propsForThisDate,

            // Everything else is for our own use
            // e.g. from "11/5/1907"
            electionYear: parseInt(date.split('/')[2]),
        });
    }

    return forApex;
}

exports.createPages = ({ actions }) => {
  const { createPage } = actions;

  const singlePropTemplate = path.resolve('./src/templates/ballot-prop.js');
  const explorerTemplate = path.resolve('./src/templates/heatmap-explorer.js');

  ballotPropsData.forEach(propObject => {
    createPage({
        path: `${propObject.election_year}/${propObject.election_month}/${propObject.election_day}/${propObject.prop_letter_or_num}`,
        component: singlePropTemplate,
        context: propObject,
    });
  });

  // Prepare data for the heatmap/explorer
  const heatmapData = prepDataForApexHeatmap(ballotPropsData);

  createPage({
    path: 'explorer',
    component: explorerTemplate,
    context: heatmapData,
  })


}
