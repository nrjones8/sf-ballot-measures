import React from "react"
import Chart from "react-apexcharts";

import Layout from "../components/layout"

const DEFAULT_MIN_YEAR = 1980;

function getFullDataPoint(dateIndex, propLetterIndex) {
  console.log(`Would get date index ${dateIndex} for prop index ${propLetterIndex}`);
}

export default class BallotPropTemplate extends React.Component {
    constructor(props) {
        super(props);

        this.getHeatmapOptions = this.getHeatmapOptions.bind(this);
        this.getTooltipText = this.getTooltipText.bind(this);
        this.state = {
          propsData: Object.values(props.pageContext).filter(p => p.electionYear >= DEFAULT_MIN_YEAR),
          options: this.getHeatmapOptions()
        };
    }

    getTooltipText(dateIndex, propLetterIndex) {
      console.log(`Tooltip text!! Would get date index ${dateIndex} for prop index ${propLetterIndex}`);
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
                  from: -50,
                  to: 0,
                  name: 'did not passs',
                  color: '#FF0000'
                },
                {
                  from: 0,
                  to: 50,
                  name: 'passed',
                  color: '#128FD9'
                }
              ]
            }
          },
        },
        tooltip: {
          custom: function({series, seriesIndex, dataPointIndex, w}) {
            // these don't really work :`(
            // date
            console.log('index ' + seriesIndex);
            console.log('corresponds to date of ' + w.globals.seriesNames[seriesIndex]);
            // prop letter
            console.log('dataPointIndex ' + dataPointIndex);

            // hrmmmm labels is wrong/off...
            console.log('corresponds to prop letter of ' + w.globals.labels[dataPointIndex]);
            console.log(w);
            getFullDataPoint(seriesIndex, dataPointIndex);
            // bleh, can't reference `this` here...
            // this.getTooltipText(seriesIndex, dataPointIndex);
            // 
            // w.seriesNames --> has the actual dates

            console.log('done w/ tooltip');
            return '<div class="arrow_box">' +
            '<span>' + 'hi!!!!!' + series[seriesIndex][dataPointIndex] + '</span>' +
            '</div>'
          },
          z: {
            show: true,
            formatter: x => 'hi',
          }
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
      // console.log(this.props.pageContext);
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
