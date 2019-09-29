import React from "react"
import Chart from "react-apexcharts";

import Layout from "../components/layout"

const DEFAULT_MIN_YEAR = 1980;

export default class BallotPropTemplate extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
          propsData: Object.values(props.pageContext).filter(p => p.electionYear >= DEFAULT_MIN_YEAR),
          options: this.getHeatmapOptions()
        };
    }

    getHeatmapOptions() {
      return {
        plotOptions: {
          heatmap: {
            shadeIntensity: 0.5,
            reverseNegativeShade: true,

            colorScale: {
              ranges: [
                {
                  from: -40,
                  to: 0,
                  name: 'did not passs',
                  color: '#FF0000'
                },
                {
                  from: 0,
                  to: 40,
                  name: 'passed',
                  color: '#128FD9'
                }
              ]
            }
          },
        },
        // TODO this looks terrible right now
        // xaxis: {
        //   position: 'top',
        //   offsetY: -40,
        // },
        dataLabels: {
          enabled: false
        },
        title: {
          text: 'HeatMap Chart with Color Range'
        },
      }
    }

    render() {
      console.log(this.props.pageContext);
        return (
            <Layout>
                <div className="row">
                  <div className="mixed-chart">
                    <Chart
                      type="heatmap"
                      options={this.state.options}
                      series={this.state.propsData}
                      // width="800"
                      height="1000"
                    />
                  </div>
                </div>
            </Layout>
        )
  }
}
