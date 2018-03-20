import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput/NumericInput';
import InfoBox from '../../common/InfoBox/InfoBox';
import { connectProperty } from './connectProperty';

class NumericProperty extends Property {
  constructor(props) {
    super(props);
    this.onChange = this.onChange.bind(this);
  }

  componentDidMount() {
    this.props.StartListening(this.props.Description.Identifier)
  }

  componentWillUnmount() {
    this.props.StopListening(this.props.Description.Identifier)
  }

  onChange(event) {
    const { value } = event.currentTarget;
    this.props.ChangeValue(value);
  }

  get descriptionPopup() {
    let { Description } = this.props;
    const { MaximumValue, MinimumValue } = Description.AdditionalData;
    const description = `${Description.description}\nMin: ${MinimumValue}, max: ${MaximumValue}`;
    return description ? (<InfoBox text={description} />) : '';
  }

  render() {
    const { Description, Value } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    return (
      <NumericInput
        value={Value}
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

NumericProperty = connectProperty(NumericProperty)
export default NumericProperty;
