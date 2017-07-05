import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';

class ViewPane extends Component {
  componentDidMount() {
    // subscribe to data
  }

  render() {
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        hej
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
