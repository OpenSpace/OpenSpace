import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingString from '../common/LoadingString/LoadingString';
import DataManager from '../../api/DataManager';

const PROPERTIES_KEY = '__allProperties';

class ViewPane extends Component {
  constructor(props) {
    super(props);
    this.state = { properties: [], hasData: false };

    this.receiveData = this.receiveData.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    DataManager.getValue(PROPERTIES_KEY, this.receiveData);
  }

  receiveData({ value }) {
    this.setState({ properties: value, hasData: true });
  }

  render() {
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        { !this.state.hasData && (
          <LoadingString loading>
            Loading...
          </LoadingString>
        )}

        { this.state.properties.map(prop => (
          <div key={prop.uri}>
            { prop.Description.Name}: "{ prop.Value }" ({ prop.Description.Identifier })
          </div>
        )) }
      </Pane>
    );
  }
}

ViewPane.propTypes = {
  closeCallback: PropTypes.func,
};

ViewPane.defaultProps = {
  closeCallback: null,
};

export default ViewPane;
