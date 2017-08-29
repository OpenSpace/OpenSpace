import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput/NumericInput';
import InfoBox from '../../common/InfoBox/InfoBox';

class NumericProperty extends Property {
  onChange(event) {
    const { value } = event.currentTarget;

    this.saveValue(value);

    // optimistic UI change!
    this.setState({ value: parseFloat(value) });
  }

  get descriptionPopup() {
    let { Description } = this.props;
    const { MaximumValue, MinimumValue } = Description.AdditionalData;
    const description = `${Description.description}\nMin: ${MinimumValue}, max: ${MaximumValue}`;
    return description ? (<InfoBox text={description} />) : '';
  }

  render() {
    const { Description } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    const { value } = this.state;
    return (
      <NumericInput
        value={value}
        label={(<span>{Description.Name} {this.descriptionPopup}</span>)}
        placeholder={Description.Name}
        onChange={this.onChange}
        step={SteppingValue}
        max={MaximumValue}
        min={MinimumValue}
        disabled={this.disabled}
      />
    );
  }
}

export default NumericProperty;
