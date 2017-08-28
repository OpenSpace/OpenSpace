import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Input from '../../common/Input/Input/Input';
import DataManager from '../../../api/DataManager';
import InfoBox from '../../common/InfoBox/InfoBox';

class Property extends Component {
  constructor(props) {
    super(props);

    this.state = { value: props.Value };

    this.onChange = this.onChange.bind(this);
    this.updateValue = this.updateValue.bind(this);
  }

  componentDidMount() {
    if (this.props.subscribe) {
      this.subscribeIfNeeded();
    }
  }

  componentWillUnmount() {
    if (this.isSubscribed) {
      DataManager.unsubscribe(this.uri, this.updateValue);
    }
  }

  onChange(event) {
    const { value } = event.currentTarget;

    this.saveValue(value);

    // optimistic UI change!
    this.setState({ value });
  }

  get uri() {
    const { Identifier } = this.props.Description;
    return Identifier;
  }

  get inputType() {
    const { Description } = this.props;
    switch (Description.Type) {
      case 'StringProperty':
      default:
        return Input;
    }
  }

  get descriptionPopup() {
    const { description } = this.props.Description;
    return description ? (<InfoBox text={description} />) : '';
  }

  get disabled() {
    return this.props.Description.MetaData.isReadOnly;
  }

  subscribeIfNeeded() {
    if (!this.isSubscribed) {
      DataManager.subscribe(this.uri, this.updateValue);
      this.isSubscribed = true;
    }
  }

  /**
   * Send value to OpenSpace
   */
  saveValue(value) {
    this.subscribeIfNeeded();
    DataManager.setValue(this.uri, value);
  }

  /**
   * New value received from OpenSpace
   * @param Value - the value
   */
  updateValue({ Value }) {
    this.setState({ value: Value });
  }

  render() {
    const { Description } = this.props;
    const { value } = this.state;
    const PropInput = this.inputType;
    const placeholder = (<span>
      { Description.Name } { this.descriptionPopup }
    </span>);
    return (
      <PropInput
        value={value}
        label={placeholder}
        placeholder={Description.Name}
        onChange={this.onChange}
        disabled={this.disabled}
      />
    );
  }
}

Property.propTypes = {
  Description: PropTypes.shape({
    Identifier: PropTypes.string,
    Name: PropTypes.string,
    MetaData: PropTypes.shape({
      isReadOnly: PropTypes.bool,
    }),
    description: PropTypes.string,
  }).isRequired,
  subscribe: PropTypes.bool,
  Value: PropTypes.oneOfType([PropTypes.string, PropTypes.number]).isRequired,
};

Property.defaultProps = {
  subscribe: false,
};

export default Property;
