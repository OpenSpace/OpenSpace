import React, { Component } from 'react';
import { ChromePicker } from 'react-color';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { changeColor } from '../../../api/Actions/transferFunctionActions.js';

class ColorPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      color: '#fff',
    };
}

  handleChangeComplete(color) {
    this.setState({color : color.hex});
    this.props.onColorChange(color.hex);
  };

  render() {
    return (
      <ChromePicker
        disableAlpha={true}
        {...this.props }
        color={ this.state.color }
        onChangeComplete={(color) => this.handleChangeComplete(color) }
      />
    );
  }
}
ColorPicker.propTypes = {
  onColorChange: PropTypes.func.isRequired,
}
export default ColorPicker;
