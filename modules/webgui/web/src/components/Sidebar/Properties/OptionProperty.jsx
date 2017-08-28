import React from 'react';
import Property from './Property';
import Select from '../../common/Input/Select/Select';

class OptionProperty extends Property {
  onChange({ value }) {
    this.saveValue(value);
    this.setState({ value });
  }

  render() {
    const { Description } = this.props;
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
        value={this.state.value}
        onChange={this.onChange}
        disabled={this.disabled}
      />
    );
  }
}

export default OptionProperty;
