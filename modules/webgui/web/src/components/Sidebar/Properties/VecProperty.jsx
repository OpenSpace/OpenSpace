import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput/NumericInput';
import Row from '../../common/Row/Row';

class VecProperty extends Property {
  static jsonToLua(json) {
    return json.replace('[', '{').replace(']', '}');
  }

  onChange(index) {
    return (event) => {
      const stateValue = JSON.parse(this.state.value);
      const { value } = event.currentTarget;
      stateValue[index] = parseFloat(value);

      this.saveValue(VecProperty.jsonToLua(JSON.stringify(stateValue)));

      // optimistic UI change!
      this.setState({ value: JSON.stringify(stateValue) });
    };
  }

  render() {
    const { Description } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    const { value } = this.state;
    const firstLabel = (<span>
      { Description.Name } { this.descriptionPopup }
    </span>);

    return (
      <Row>
        { JSON.parse(value).map((component, index) => (
          <NumericInput
            value={component}
            placeholder={index === 0 ? firstLabel : ' '}
            onChange={this.onChange(index)}
            step={SteppingValue[index]}
            max={MaximumValue[index]}
            min={MinimumValue[index]}
          />
        ))}
      </Row>
    );
  }
}

export default VecProperty;
