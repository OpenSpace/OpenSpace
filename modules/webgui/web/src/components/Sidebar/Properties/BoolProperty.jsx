import React from 'react';
import Property from './Property';
import Checkbox from '../../common/Input/Checkbox/Checkbox';
import { connectProperty } from './connectProperty';

class BoolProperty extends Property {
  constructor(props) {
    super(props);
    this.onChange = this.onChange.bind(this);
  }

  onChange(value) {
    // convert to Lua compatible value
    this.props.ChangeValue(value ? '1' : '0');
  }

  componentDidMount() {
    this.props.StartListening(this.props.Description.Identifier);
  }

  componentWillUnmount() {
    this.props.StopListening(this.props.Description.Identifier);
  }

  render() {
    const { Description, Value } = this.props;
    return (
      <Checkbox
        checked={Value === 'true'}
        label={(<span>{Description.Name} {this.descriptionPopup}</span>)}
        onChange={this.onChange}
        disabled={this.disabled}
      />
    );
  }
}

BoolProperty = connectProperty(BoolProperty);
export default BoolProperty;
