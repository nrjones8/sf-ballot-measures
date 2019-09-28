// Thank you https://dev.to/ashleemboyer/build-a-multi-page-gatsby-site-from-json-3kp
const path = require('path');
const ballotPropsData = require('../data/ballot_measure_history.json');

exports.createPages = ({ actions }) => {
  const { createPage } = actions;

  const template = path.resolve('./src/templates/ballot-prop.js');

  ballotPropsData.forEach(propObject => {
    createPage({
        path: `${propObject.election_year}/${propObject.election_month}/${propObject.election_day}/${propObject.prop_letter_or_num}`,
        component: template,
        context: propObject,
    });
  });
}
