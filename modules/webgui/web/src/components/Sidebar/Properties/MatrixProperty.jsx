import React from 'react';
import Property from './Property';
import NumericInput from '../../common/Input/NumericInput/NumericInput';
import Row from '../../common/Row/Row';
import styles from './Property.scss';
import { connectProperty } from './connectProperty';

class MatrixProperty extends Property {
  constructor(props) {
    super(props);
    this.onChange = this.onChange.bind(this);
  }

  static jsonToLua(json) {
    return json.replace('[', '').replace(']', '');
  }

  componentDidMount() {
    this.props.StartListening(this.props.Description.Identifier);
  }

  componentWillUnmount() {
    this.props.StopListening(this.props.Description.Identifier);
  }

  onChange(index) {
    return (event) => {
      const stateValue = JSON.parse(`[${this.props.Value}]`);
      const { value } = event.currentTarget;
      stateValue[index] = parseFloat(value);

      this.props.ChangeValue(JSON.stringify(stateValue));
    };
  }

  render() {
    const { Description, Value } = this.props;
    const { SteppingValue, MaximumValue, MinimumValue } = Description.AdditionalData;
    const firstLabel = (<span>
      { Description.Name } { this.descriptionPopup }
    </span>);

    // Convert the flat array into a 2D N*N array representing the matrix. Add '[' and ']' to
    // compensate for formatting from the server.
    const values = JSON.parse(`[${Value}]`)
      .map((value, index) => ({
        key: `${Description.Name}-${index}`,
        value: parseFloat(value),
        index,
      }));
    // Find N
    const matrixSize = Math.sqrt(values.length);
    // actually convert into N arrays of N length
    const groups = Array.from(new Array(matrixSize), () => values.splice(0, matrixSize));

    // eslint-disable react/no-array-index-key
    return (
      <div className={styles.matrixProperty}>
        { groups.map((group, index) => (
          <Row key={`row-${index}`}>
            { group.map(comp => (
              <NumericInput
                key={comp.key}
                value={comp.value}
                label={comp.index === 0 ? firstLabel : ' '}
                placeholder={`Value ${comp.index}`}
                onChange={this.onChange(comp.index)}
                step={SteppingValue[comp.index] || 0.01}
                max={MaximumValue[comp.index] || 100}
                min={MinimumValue[comp.index] || -100}
                disabled={this.disabled}
                noTooltip
              />
            ))}
          </Row>
        ))}
      </div>
    );
  }
}

MatrixProperty = connectProperty(MatrixProperty);
export default MatrixProperty;
