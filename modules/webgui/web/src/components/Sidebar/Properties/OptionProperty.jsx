import React from 'react';
import Property from './Property';
import Select from '../../common/Input/Select/Select';
import { connectProperty } from './connectProperty';

class OptionProperty extends Property {
  constructor(props) {
    super(props);
    this.onChange = this.onChange.bind(this);
  }

  onChange({ value }) {
    this.props.ChangeValue(value);
  }

  componentDidMount() {
    this.props.StartListening(this.props.Description.Identifier)
  }

  componentWillUnmount() {
    this.props.StopListening(this.props.Description.Identifier)
  }

  render() {
    const { Description, Value } = this.props;
    const label = (
      <span>
        { Description.Name } { this.descriptionPopup }
      </span>
    );
    const options = Description.AdditionalData.Options
      .map(option => ({ label: Object.values(option)[0], value: Object.keys(option)[0] }));
    return (
      <Select
        label={label}
        options={options}
        value={Value}
        onChange={this.onChange}
        disabled={this.disabled}
      />
    );
  }
}
OptionProperty = connectProperty(OptionProperty);

export default OptionProperty;
