import React from "react"
import Chart from "react-apexcharts";

import Layout from "../components/layout"


function prepSeriesData(ballotPropObj) {
    return [
        {
            name: "Yes",
            data: [ballotPropObj.num_yes_votes]
        },
        {
            name: "No",
            data: [ballotPropObj.num_no_votes]
        }
    ];
}

export default class BallotPropTemplate extends React.Component {
    constructor(props) {
        super(props);

        const seriesData = prepSeriesData(props.pageContext);
        console.log(seriesData);
        this.state = {
          options: {
            chart: {
              id: "basic-bar",
              stacked: true,
              stackType: '100%',
              // Nobody wants to download a barchart with one bar on it...
              toolbar: {
                show: false,
              }
            },
            plotOptions: {
                bar: {
                    horizontal: true,
                }
            },
          },
          series: seriesData,
        };
    }

    render() {
        const propData = this.props.pageContext;
        return (
            <Layout>
                <h1>Prop {propData.prop_letter_or_num} from {propData.election_date} </h1>
                <h2>{ propData.prop_title }</h2>
                <h3>{ propData.pass_or_fail === 'P' ? 'Passed' : 'Failed' } ({ propData.pct_yes_votes} % Yes)</h3>
                <div>
                    { propData.description }
                </div>

                <div className="row">
                  <div className="mixed-chart">
                    <Chart
                      type="bar"
                      options={this.state.options}
                      series={this.state.series}
                      width="300"
                    />
                  </div>
                </div>
            </Layout>
        )
  }
}
