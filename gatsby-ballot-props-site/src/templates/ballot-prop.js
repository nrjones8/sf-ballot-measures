import React from "react"
import Layout from "../components/layout"


export default class BallotPropTemplate extends React.Component {
    render() {
        const propData = this.props.pageContext;
        return (
            <Layout>
                <h1>Prop {propData.prop_letter_or_num} from {propData.election_date} </h1>
                <h3>{ propData.pass_or_fail == 'P' ? 'Passed' : 'Failed' } ({ propData.pct_yes_votes} % Yes)</h3>
                <div>
                    { propData.description }
                </div>
            </Layout>
        )
  }
}
