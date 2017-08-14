import React from 'react';
import Property from './Property';
import Checkbox from '../../common/Input/Checkbox';

class BoolProperty extends Property {
  onChange(value) {
    this.saveValue(value ? '1' : '0');

    // optimistic UI change!
    this.setState({ value });
  }

  render() {
    const { Description } = this.props;
    const { value } = this.state;
    return (
      <Checkbox checked={value} label={Description.Name} onChange={this.onChange} />
    );
  }
}

export default BoolProperty;
