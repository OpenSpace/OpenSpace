import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../../utils/helpers';
import styles from './Checkbox.scss';
import Input from '../Input/Input';

class Checkbox extends Component {
  constructor(props) {
    super(props);

    this.state = {
      checked: props.checked || props.value,
      id: `checkbox-${Input.nextId}`,
    };

    this.onChange = this.onChange.bind(this);
  }

  componentWillReceiveProps({ checked, value }) {
    this.setState({ checked: checked || value });
  }

  onChange(event) {
    const { checked } = event.currentTarget;
    this.setState({ checked });

    this.props.onChange(checked);
  }

  render() {
    const inheritProps = excludeKeys(this.props, 'wide id label onChange type left');
    const { label, wide, left } = this.props;
    const { checked, id } = this.state;

    return (
      <div className={`${styles.wrapper} ${wide ? styles.wide : ''} ${left ? styles.left : ''}`}>
        <input
          {...inheritProps}
          id={id}
          type="checkbox"
          checked={checked}
          onChange={this.onChange}
        />
        <label htmlFor={id}>{ label }</label>
      </div>
    );
  }
}

Checkbox.propTypes = {
  checked: PropTypes.bool,
  onChange: PropTypes.func,
  label: PropTypes.node.isRequired,
  left: PropTypes.bool,
  value: PropTypes.bool,
  wide: PropTypes.bool,
};

Checkbox.defaultProps = {
  checked: false,
  left: false,
  onChange: () => {},
  value: false,
  wide: true,
};

export default Checkbox;
