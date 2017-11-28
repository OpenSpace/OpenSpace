import React, { Component } from 'react';
import { SketchPicker } from 'react-color';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import { changeColor } from './actions';

let createHandlers = function(dispatch) {
  let ChangeColor = function(data) {
    dispatch(changeColor(data))
  };
  return {
    ChangeColor,
  };
}

class ColorPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      color: '#fff',
    };

    this.handlers = createHandlers(this.props.dispatch);
}

  handleChangeComplete(color) {
    this.handlers.ChangeColor(color.hex);
    this.setState({color : color.hex});
    this.props.onColorChange(color.hex);
  };

  render() {
    return (
      <SketchPicker
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
export default connect()(ColorPicker);
