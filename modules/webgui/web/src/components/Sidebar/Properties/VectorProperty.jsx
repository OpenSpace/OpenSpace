import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput/NumericInput';
import Row from '../../common/Row/Row';
import styles from './Property.scss';
import { connectProperty } from './connectProperty';

class VectorProperty extends Property {
  static jsonToLua(json) {
    return json.replace('[', '').replace(']', '');
  }

  componentDidMount() {
    this.props.StartListening(this.props.Description.Identifier)
  }

  componentWillUnmount() {
    this.props.StopListening(this.props.Description.Identifier)
  }

  onChange(index) {
    return (event) => {
      const stateValue = JSON.parse(this.props.Value);
      const { value } = event.currentTarget;

      stateValue[index] = parseFloat(value);
      this.props.ChangeValue( JSON.stringify(stateValue));
    };
  }

  render() {
    const { Description } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    const firstLabel = (<span>
      { Description.Name } { this.descriptionPopup }
    </span>);

    // eslint-disable-next-line react/no-array-index-key
    const values = JSON.parse(this.props.Value)
      .map((value, index) => ({ key: `${Description.Name}-${index}`, value }));

    return (
      <Row className={styles.vectorProperty}>
        { values.map((component, index) => (
          <NumericInput
            key={component.key}
            value={component.value}
            label={index === 0 ? firstLabel : ' '}
            placeholder={`Value ${index}`}
            onChange={this.onChange(index)}
            step={SteppingValue[index]}
            max={MaximumValue[index]}
            min={MinimumValue[index]}
            disabled={this.disabled}
          />
        ))}
      </Row>
    );
  }
}
VectorProperty = connectProperty(VectorProperty);

export default VectorProperty;
