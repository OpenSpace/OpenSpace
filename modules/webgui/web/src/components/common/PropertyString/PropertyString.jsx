import React, { Component } from 'react';
import PropTypes from 'prop-types';
import LoadingString from '../LoadingString/LoadingString';
import DataManager from '../../../api/DataManager';

/**
 * display the value of a property in an effortless way
 */
class PropertyString extends Component {
  constructor(props) {
    super(props);

    this.state = { value: props.defaultValue, hasValue: false };

    this.callback = this.callback.bind(this);
  }

  componentDidMount() {
    const { method, propertyKey } = this.props;
    // get or subscribe to property
    DataManager[method](propertyKey, this.callback);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(this.props.propertyKey, this.callback);
  }

  callback({ Value: value }) {
    // console.log(value);
    this.setState({ value, hasValue: true });
  }

  render() {
    return (
      <LoadingString loading={!this.state.hasValue}>
        { this.state.value }
      </LoadingString>
    );
  }
}

PropertyString.propTypes = {
  defaultValue: PropTypes.string,
  propertyKey: PropTypes.string.isRequired,
  method: PropTypes.oneOf(['getValue', 'subscribe']),
};

PropertyString.defaultProps = {
  defaultValue: 'loading',
  method: 'getValue',
};

export default PropertyString;
