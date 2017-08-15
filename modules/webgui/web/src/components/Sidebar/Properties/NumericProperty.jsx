import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput';

class NumericProperty extends Property {
  onChange(event) {
    const { value } = event.currentTarget;

    this.saveValue(value);

    // optimistic UI change!
    this.setState({ value: parseFloat(value) });
  }

  render() {
    const { Description } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    const { value } = this.state;
    return (
      <NumericInput
        value={value}
        placeholder={Description.Name}
        onChange={this.onChange}
        step={SteppingValue}
        max={MaximumValue}
        min={MinimumValue}
      />
    );
  }
}

export default NumericProperty;
