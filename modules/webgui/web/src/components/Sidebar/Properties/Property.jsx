import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Input from '../../common/Input/Input/Input';
import DataManager from '../../../api/DataManager';
import InfoBox from '../../common/InfoBox/InfoBox';

class Property extends Component {
  constructor(props) {
    super(props);
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

  render() {
    const { Description, Value } = this.props;
    const PropInput = this.inputType;
    const placeholder = (<span>
      { Description.Name } { this.descriptionPopup }
    </span>);
    return (
      <PropInput
        value={Value}
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
